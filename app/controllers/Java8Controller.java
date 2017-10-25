package controllers;

import play.Logger;
import play.libs.ws.WS;
import play.libs.ws.WSResponse;
import play.mvc.Controller;
import play.mvc.Result;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletionStage;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.lang.String.format;
import static java.lang.System.out;

public class Java8Controller extends Controller {

  private static final List<String> names = Arrays.asList("Jim", "Dean", "Kunal");

  private static final Function<String, String> lower = str -> str.toLowerCase();

  private static final Function<String, Integer> strlen = str -> str.length();

  private static final Function<String, List<Character>> explode = str -> {
    final List<Character> chars = new ArrayList<>();
    for (char c : str.toCharArray()) {
      chars.add(Character.valueOf(c));
    }
    return chars;
  };

  private static final Function<String, Stream<Character>> explodeToStream =
          str -> explode.apply(str).stream();

  public Result index() {
    final List<String> loweredNames = names.stream().map(lower).collect(Collectors.toList());
    out.println(format("%s.map(lower) yields %s", names, loweredNames));

    final List<Integer> lengthsOfNames = names.stream().map(strlen).collect(Collectors.toList());
    out.println(format("%s.map(strlen) yields %s", names, lengthsOfNames));

    final List<List<Character>> explodedNames =
            names.stream().map(explode).collect(Collectors.toList());
    out.println(format("%s.map(explode) yields %s", names, explodedNames));

    final List<Character> flattenedExplodedNames =
            names.stream().flatMap(explodeToStream).collect(Collectors.toList());
    out.println(format("%s.map(explodeToStream) yields %s", names, flattenedExplodedNames));

    return ok(Arrays.asList("Hello World").stream().map(lower).collect(Collectors.toList()).get(0));
  }

  public CompletionStage<Result> proxy() {
    final CompletionStage<WSResponse> responsePromise = WS.url("http://example.com").get();

    Logger.info("Before map");
    final CompletionStage<Result> resultPromise = responsePromise.thenApplyAsync((wsResponse) -> {
      Logger.info("Within map");
      response().setContentType(wsResponse.getHeader("Content-Type"));
      return ok(wsResponse.getBody());
    });

    Logger.info("After map");
    return resultPromise;
  }

  public CompletionStage<Result> parallel() {
    final long start = System.currentTimeMillis();
    final Function<WSResponse, Long> getLatency = resp -> System.currentTimeMillis() - start;

    CompletionStage<Long> googleLatency = WS.url("http://google.com").get().thenApplyAsync(getLatency);
    CompletionStage<Long> yahooLatency = WS.url("http://yahoo.com").get().thenApplyAsync(getLatency);

    return googleLatency.thenComposeAsync(googleResponseTime ->
            yahooLatency.thenApplyAsync(yahooResponseTime ->
                    ok(format("Google response time:  %d; Yahoo response time:  %d",
                            googleResponseTime, yahooResponseTime)))
    );
  }

  private static final String paramsFromFoo(Object anything) {
    return "?bar=baz";
  }

  public CompletionStage<Result> sequential() {
    final CompletionStage<WSResponse> foo = WS.url("http://www.foo.com").get();

    return foo.thenComposeAsync(fooResponse -> {
      // Use data in fooResponse to build the second request
      final CompletionStage<WSResponse> bar = WS.url("http://www.bar.com" + paramsFromFoo(fooResponse)).get();

      return bar.thenApplyAsync(barResponse -> {
        // Now you can use barResponse and fooResponse to build a Result
        return ok(format("response from foo.com is %s & from bar.com is %s",
                fooResponse.getStatusText(), barResponse.getStatusText()));
      });
    });
  }

  // Handle Exceptions in Futures by logging them and returning a fallback value
  private <T> CompletionStage<T> withErrorHandling(CompletionStage<T> promise, T fallback) {
    return promise.exceptionally(throwable -> {
      Logger.error("Something went wrong!", throwable);
      return fallback;
    });
  }

  public CompletionStage<Result> checkHostName(String hostName) {
    // try using "thisdomaindoesnotexist"
    final CompletionStage<String> myPromise = WS.url(format("http://www.%s.com", hostName)).get()
            .thenApplyAsync(response -> response.getStatusText());

    final CompletionStage<String> myPromiseWithFallback = withErrorHandling(myPromise, "fallback value");

    // str either contains the result of myFuture's async I/O or
    // "fallback value" if any Exception was thrown
    return myPromiseWithFallback.thenApplyAsync(s -> ok(s));
  }
}
