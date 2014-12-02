package controllers;

import play.Logger;
import play.libs.F;
import play.libs.F.Function;
import play.libs.WS;
import play.mvc.Controller;
import play.mvc.Result;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import static java.lang.System.out;
import static java.util.Arrays.asList;

public class LinkedInJavaController extends Controller {

    static final List<String> names = asList("Jim", "Dean", "Kunal");

    static final Function<String, String> lower = new Function<String, String>() {
        @Override
        public String apply(String arg0) throws Throwable {
            return arg0.toLowerCase();
        }
    };

    static final Function<String, Integer> strlen = new Function<String, Integer>() {
        @Override
        public Integer apply(String str) throws Throwable {
            return Integer.valueOf(str.length());
        }
    };

    static final Function<String, List<Character>> explode = new Function<String, List<Character>>() {
        @Override
        public List<Character> apply(String arg0) throws Throwable {
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

    public static Result index() throws Throwable {
        final Collection<String> loweredNames = map(lower, names);
        out.println(loweredNames);

        final Collection<Integer> lengthOfNames = map(strlen, names);
        out.println(lengthOfNames);

        final Collection<List<Character>> explodedNames = map(explode, names);
        out.println(explodedNames);

        final Collection<Character> flattenedExplodedNames = flatMap(explode, names);
        out.println(flattenedExplodedNames);


        return ok(map(lower, "Hello World"));
    }

    public static F.Promise<Result> proxy() {
        final F.Promise<WS.Response> responsePromise = WS.url("http://example.com").get();

        Logger.info("Before map");
        final F.Promise<Result> resultPromise = responsePromise.map(
                new Function<WS.Response, Result>() {
                    @Override
                    public Result apply(WS.Response wsResponse) throws Throwable {
                        Logger.info("Within map");
                        response().setContentType(wsResponse.getHeader("Content-Type"));
                        return ok(wsResponse.getBody());
                    }
                }
        );
        Logger.info("After map");
        return resultPromise;
    }

}
