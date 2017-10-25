package controllers;

import play.Logger;
import play.libs.ws.WS;
import play.libs.ws.WSResponse;
import play.mvc.Controller;
import play.mvc.Result;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletionStage;
import java.util.function.Function;

import static java.lang.String.format;
import static java.lang.System.out;
import static java.util.Arrays.asList;

public class JavaController extends Controller {

  static final List<String> names = asList("Jim", "Dean", "Kunal");

  static final Function<String, String> lower = new Function<String, String>() {
    @Override
    public String apply(String arg0) {
      return arg0.toLowerCase();
    }
  };

  static final Function<String, Integer> strlen = new Function<String, Integer>() {
    @Override
    public Integer apply(String str) {
      return str.length();
    }
  };

  static final Function<String, List<Character>> explode = new Function<String, List<Character>>() {
    @Override
    public List<Character> apply(String arg0) {
      final List<Character> result = new ArrayList<>();
      for (char c : arg0.toCharArray()) {
        result.add(c);
      }
      return result;
    }
  };

  static <R, A> Collection<R> map(Function<A, R> f, Collection<A> c) throws Throwable {
    final Collection<R> result = new ArrayList<>();
    for (A a : c) {
      result.add(f.apply(a));
    }
    return result;
  }

  static <R, A, C extends Collection<R>> Collection<R> flatMap(Function<A, C> f, Collection<A> c) throws Throwable {
    final Collection<R> result = new ArrayList<>();
    for (A a : c) {
      for (R r : f.apply(a)) {
        result.add(r);
      }
    }
    return result;
  }

  static <R, A> R map(Function<A, R> f, A c) throws Throwable {
    return f.apply(c);
  }

  public Result index() throws Throwable {
    final Collection<String> loweredNames = map(lower, names);
    out.println(format("%s.map(lower) yields %s", names, loweredNames));

    final Collection<Integer> lengthsOfNames = map(strlen, names);
    out.println(format("%s.map(strlen) yields %s", names, lengthsOfNames));

    final Collection<List<Character>> explodedNames = map(explode, names);
    out.println(format("%s.map(explode) yields %s", names, explodedNames));

    final Collection<Character> flattenedExplodedNames = flatMap(explode, names);
    out.println(format("%s.map(explodeToStream) yields %s", names, flattenedExplodedNames));

    return ok(map(lower, "Hello World"));
  }

  public CompletionStage<Result> proxy() {
    final CompletionStage<WSResponse> responsePromise = WS.url("http://example.com").get();

    Logger.info("Before map");
    final CompletionStage<Result> resultPromise = responsePromise.thenApplyAsync(
            new Function<WSResponse, Result>() {
              @Override
              public Result apply(WSResponse wsResponse) {
                Logger.info("Within map");
                response().setContentType(wsResponse.getHeader("Content-Type"));
                return ok(wsResponse.getBody());
              }
            }
    );

    Logger.info("After map");
    return resultPromise;
  }

  public CompletionStage<Result> parallel() {
    final long start = System.currentTimeMillis();
    final Function<WSResponse, Long> getLatency = new Function<WSResponse, Long>() {
      @Override
      public Long apply(WSResponse response) {
        return System.currentTimeMillis() - start;
      }
    };

    CompletionStage<Long> googleLatency = WS.url("http://google.com").get().thenApplyAsync(getLatency);
    CompletionStage<Long> yahooLatency = WS.url("http://yahoo.com").get().thenApplyAsync(getLatency);

    return googleLatency.thenComposeAsync(new Function<Long, CompletionStage<Result>>() {
      @Override
      public CompletionStage<Result> apply(Long googleResponseTime) {
        return yahooLatency.thenApplyAsync(new Function<Long, Result>() {
          @Override
          public Result apply(Long yahooResponseTime) {
            return ok(format("Google response time:  %d; Yahoo response time:  %d",
                    googleResponseTime, yahooResponseTime));
          }
        });
      }
    });
  }

  private static final String paramsFromFoo(Object anything) {
    return "?bar=baz";
  }

  public CompletionStage<Result> sequential() {
    final CompletionStage<WSResponse> foo = WS.url("http://www.foo.com").get();

    return foo.thenComposeAsync(new Function<WSResponse, CompletionStage<Result>>() {
      @Override
      public CompletionStage<Result> apply(WSResponse fooResponse) {
        // Use data in fooResponse to build the second request
        final CompletionStage<WSResponse> bar = WS.url("http://www.bar.com/" + paramsFromFoo(fooResponse))
                .get();

        return bar.thenApplyAsync(new Function<WSResponse, Result>() {
          @Override
          public Result apply(WSResponse barResponse) {
            // Now you can use barResponse and fooResponse to build a Result
            return ok(format("response from foo.com is %s & from bar.com is %s",
                    fooResponse.getStatusText(), barResponse.getStatusText()));
          }
        });
      }
    });
  }

  // Handle Exceptions in Futures by logging them and returning a fallback value
  private <T> CompletionStage<T> withErrorHandling(CompletionStage<T> promise, T fallback) {
    return promise.exceptionally(new Function<Throwable, T>() {
      @Override
      public T apply(Throwable throwable) {
        Logger.error("Something went wrong!", throwable);
        return fallback;
      }
    });
  }

  public CompletionStage<Result> checkHostName(String hostName) {
    // try using "thisdomaindoesnotexist"
    final CompletionStage<String> myPromise = WS.url(format("http://www.%s.com", hostName)).get()
            .thenApplyAsync(new Function<WSResponse, String>() {
              @Override
              public String apply(WSResponse response) {
                return response.getStatusText();
              }
            });

    final CompletionStage<String> myPromiseWithFallback = withErrorHandling(myPromise, "fallback value");

    // str either contains the result of myFuture's async I/O or
    // "fallback value" if any Exception was thrown
    return myPromiseWithFallback.thenApplyAsync(new Function<String, Result>() {
      @Override
      public Result apply(String s) {
        return ok(s);
      }
    });
  }

}
