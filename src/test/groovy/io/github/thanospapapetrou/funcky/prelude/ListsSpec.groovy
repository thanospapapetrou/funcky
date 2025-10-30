package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.FunckyJavaConverter
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException
import io.github.thanospapapetrou.funcky.runtime.prelude.Lists
import spock.lang.Unroll

import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.FALSE
import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.TRUE
import static io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType.FUNCTION
import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.LIST
import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.STRING
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.BOOLEAN
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.CHARACTER
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.NUMBER

class ListsSpec extends BaseSpec {
    @Unroll('Test head (expression: #expression)')
    def 'Test head'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                           || result
        '"funcky:lists".head'                                                                                                                                                                || new Lists().$head
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".head)))'                                                             || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".type "funcky:lists".head))'                                                                                       || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".head))) ("funcky:types".range ("funcky:types".type "funcky:lists".head))' || TRUE
        '"funcky:lists".head [0]'                                                                                                                                                            || new FunckyNumber(0.0G)
        '"funcky:lists".head [0, 1]'                                                                                                                                                         || new FunckyNumber(0.0G)
        '"funcky:lists".head "a"'                                                                                                                                                            || new FunckyCharacter('a' as char)
        '"funcky:lists".head "ab"'                                                                                                                                                           || new FunckyCharacter('a' as char)
    }

    @Unroll('Test head (runtime error, expression: #expression)')
    def 'Test head (runtime error)'(final String expression) {
        when:
        engine.eval(expression)
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(Lists.ERROR_HEAD)
        where:
        expression << ['"funcky:lists".head []', '"funcky:lists".head ""']
    }

    @Unroll('Test tail (expression: #expression)')
    def 'Test tail'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                    || result
        '"funcky:lists".tail'                                                                                                                                                                                         || new Lists().$tail
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".tail)))'                                                                                      || TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".tail)))'                                                                                       || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".tail))) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".tail)))' || TRUE
        '"funcky:lists".tail [0]'                                                                                                                                                                                     || new FunckyJavaConverter().convert(List.of())
        '"funcky:lists".tail [0, 1]'                                                                                                                                                                                  || new FunckyJavaConverter().convert(List.of(1.0G))
        '"funcky:lists".tail "a"'                                                                                                                                                                                     || new FunckyJavaConverter().convert('')
        '"funcky:lists".tail "ab"'                                                                                                                                                                                    || new FunckyJavaConverter().convert('b')
    }

    @Unroll('Test tail (runtime error, expression: #expression)')
    def 'Test tail (runtime error)'(final String expression) {
        when:
        engine.eval(expression)
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(Lists.ERROR_TAIL)
        where:
        expression << ['"funcky:lists".tail []', '"funcky:lists".tail ""']
    }

    @Unroll('Test prepend (expression: #expression)')
    def 'Test prepend'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                 || result
        '"funcky:lists".prepend'                                                                                                                                                                                                                   || new Lists().$prepend
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".prepend)))'                                                                                                                || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".prepend)))'                                                                                                                  || TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".prepend))))'                                                                                          || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".prepend))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".prepend)))'                         || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".prepend))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".prepend))))' || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type ("funcky:lists".prepend [])))'                                                                                                                                    || TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".prepend []))))'                                                                                                            || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type ("funcky:lists".prepend []))) ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".prepend []))))'                                       || FALSE
        '"funcky:types".type ("funcky:lists".prepend [0])'                                                                                                                                                                                         || FUNCTION(NUMBER, LIST(NUMBER))
        '"funcky:types".type ("funcky:lists".prepend "")'                                                                                                                                                                                          || FUNCTION(CHARACTER, STRING)
        '"funcky:commons".string ("funcky:lists".prepend ("funcky:commons".error "foo"))'                                                                                                                                                          || new FunckyJavaConverter().convert('"funcky:lists".prepend ("funcky:commons".error "foo")')
        '"funcky:lists".prepend [] 0'                                                                                                                                                                                                              || new FunckyJavaConverter().convert(List.of(0.0G))
        '"funcky:lists".prepend [1] 0'                                                                                                                                                                                                             || new FunckyJavaConverter().convert(List.of(0.0G, 1.0G))
        '"funcky:lists".prepend "" \'a\''                                                                                                                                                                                                          || new FunckyJavaConverter().convert('a')
        '"funcky:lists".prepend "b" \'a\''                                                                                                                                                                                                         || new FunckyJavaConverter().convert('ab')
    }

    @Unroll('Test append (expression: #expression)')
    def 'Test append'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".append)'                                                                                                                                                                   || FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".append)))'                                                                                                               || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".append)))'                                                                                                                 || TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".append))))'                                                                                         || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".append))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".append)))'                         || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".append))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".append))))' || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type ("funcky:lists".append [])))'                                                                                                                                   || TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".append []))))'                                                                                                           || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type ("funcky:lists".append []))) ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".append []))))'                                       || FALSE
        '"funcky:types".type ("funcky:lists".append [0])'                                                                                                                                                                                        || FUNCTION(NUMBER, LIST(NUMBER))
        '"funcky:types".type ("funcky:lists".append "")'                                                                                                                                                                                         || FUNCTION(CHARACTER, STRING)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".append ("funcky:commons".error "foo")))'                                                                                                                                  || FALSE
        '"funcky:lists".append [] 0'                                                                                                                                                                                                             || new FunckyJavaConverter().convert(List.of(0.0G))
        '"funcky:lists".append [1] 0'                                                                                                                                                                                                            || new FunckyJavaConverter().convert(List.of(1.0G, 0.0G))
        '"funcky:lists".append "" \'a\''                                                                                                                                                                                                         || new FunckyJavaConverter().convert('a')
        '"funcky:lists".append "b" \'a\''                                                                                                                                                                                                        || new FunckyJavaConverter().convert('ba')
    }

    @Unroll('Test concat (expression: #expression)')
    def 'Test concat'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".concat)'                                                                                                                                                                    || FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".concat)))'                                                                                                                || TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))'                                                                                         || TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))'                                                                                          || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".concat))) ("funcky:types".element ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))' || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".concat))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))'  || TRUE
        '"funcky:types".type ("funcky:lists".concat [0])'                                                                                                                                                                                         || FUNCTION(LIST(NUMBER), LIST(NUMBER))
        '"funcky:types".type ("funcky:lists".concat "")'                                                                                                                                                                                          || FUNCTION(STRING, STRING)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".concat ("funcky:commons".error "foo")))'                                                                                                                                   || FALSE
        '"funcky:lists".concat [] []'                                                                                                                                                                                                             || new FunckyJavaConverter().convert(List.of())
        '"funcky:lists".concat [] [0]'                                                                                                                                                                                                            || new FunckyJavaConverter().convert(List.of(0.0G))
        '"funcky:lists".concat [0] []'                                                                                                                                                                                                            || new FunckyJavaConverter().convert(List.of(0.0G))
        '"funcky:lists".concat [0] [1]'                                                                                                                                                                                                           || new FunckyJavaConverter().convert(List.of(0.0G, 1.0G))
        '"funcky:lists".concat "" ""'                                                                                                                                                                                                             || new FunckyJavaConverter().convert('')
        '"funcky:lists".concat "" "a"'                                                                                                                                                                                                            || new FunckyJavaConverter().convert('a')
        '"funcky:lists".concat "a" ""'                                                                                                                                                                                                            || new FunckyJavaConverter().convert('a')
        '"funcky:lists".concat "a" "b"'                                                                                                                                                                                                           || new FunckyJavaConverter().convert('ab')
    }

    @Unroll('Test singleton (expression: #expression)')
    def 'Test singleton'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                     || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".singleton)'                                                                                                                      || FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:lists".singleton))'                                                                                           || TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".singleton)))'                                                                   || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:lists".singleton)) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".singleton)))' || TRUE
        '"funcky:lists".singleton 0'                                                                                                                                                                   || new FunckyJavaConverter().convert(List.of(0.0G))
        '"funcky:lists".singleton \'a\''                                                                                                                                                               || new FunckyJavaConverter().convert('a')
    }

    @Unroll('Test empty (expression: #expression)')
    def 'Test empty'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                || result
        '"funcky:commons".string "funcky:lists".empty'                                                                            || new FunckyJavaConverter().convert('"funcky:commons".equal []')
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".empty)))' || TRUE
        '"funcky:types".range ("funcky:types".type "funcky:lists".empty)'                                                         || BOOLEAN
        '"funcky:lists".empty []'                                                                                                 || TRUE
        '"funcky:lists".empty [0]'                                                                                                || FALSE
        '"funcky:lists".empty ""'                                                                                                 || TRUE
        '"funcky:lists".empty "a"'                                                                                                || FALSE
    }

    @Unroll('Test size (expression: #expression)')
    def 'Test size'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".size)'                                                     || FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".size)))' || TRUE
        '"funcky:types".range ("funcky:types".type "funcky:lists".size)'                                                         || NUMBER
        '"funcky:lists".size []'                                                                                                 || new FunckyNumber(0.0G)
        '"funcky:lists".size [0]'                                                                                                || new FunckyNumber(1.0G)
        '"funcky:lists".size ""'                                                                                                 || new FunckyNumber(0.0G)
        '"funcky:lists".size "a"'                                                                                                || new FunckyNumber(1.0G)
    }

    @Unroll('Test index (expression: #expression)')
    def 'Test index'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                     || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".index)'                                                                                                                                          || FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".index)))'                                                                                      || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".index)))'                                                                                        || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".index))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".index)))' || TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".index))'                                                                                                                       || NUMBER
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".index [0]))'                                                                                                                                       || NUMBER
        '"funcky:types".range ("funcky:types".type ("funcky:lists".index [0]))'                                                                                                                                        || NUMBER
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".index "a"))'                                                                                                                                       || CHARACTER
        '"funcky:types".range ("funcky:types".type ("funcky:lists".index "a"))'                                                                                                                                        || NUMBER
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".index ("funcky:commons".error "foo")))'                                                                                                         || FALSE
        '"funcky:lists".index [] 0'                                                                                                                                                                                    || new FunckyNumber(-1.0G)
        '"funcky:lists".index [0] 0'                                                                                                                                                                                   || new FunckyNumber(0.0G)
        '"funcky:lists".index [0] 1'                                                                                                                                                                                   || new FunckyNumber(-1.0G)
        '"funcky:lists".index [0, 1] 0'                                                                                                                                                                                || new FunckyNumber(0.0G)
        '"funcky:lists".index [0, 1] 1'                                                                                                                                                                                || new FunckyNumber(1.0G)
        '"funcky:lists".index "" \'a\''                                                                                                                                                                                || new FunckyNumber(-1.0G)
        '"funcky:lists".index "a" \'a\''                                                                                                                                                                               || new FunckyNumber(0.0G)
        '"funcky:lists".index "a" \'b\''                                                                                                                                                                               || new FunckyNumber(-1.0G)
        '"funcky:lists".index "ab" \'a\''                                                                                                                                                                              || new FunckyNumber(0.0G)
        '"funcky:lists".index "ab" \'b\''                                                                                                                                                                              || new FunckyNumber(1.0G)
        '"funcky:lists".index [] ("funcky:commons".error "foo")'                                                                                                                                                       || new FunckyNumber(-1.0G)
    }

    @Unroll('Test contains (expression: #expression)')
    def 'Test contains'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                           || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".contains)'                                                                                                                                             || FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".contains)))'                                                                                         || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".contains)))'                                                                                           || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".contains))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".contains)))' || TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".contains))'                                                                                                                          || BOOLEAN
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".contains [0]))'                                                                                                                                          || NUMBER
        '"funcky:types".range ("funcky:types".type ("funcky:lists".contains [0]))'                                                                                                                                           || BOOLEAN
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".contains "a"))'                                                                                                                                          || CHARACTER
        '"funcky:types".range ("funcky:types".type ("funcky:lists".contains "a"))'                                                                                                                                           || BOOLEAN
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".contains ("funcky:commons".error "foo")))'                                                                                                            || FALSE
        '"funcky:lists".contains [] 0'                                                                                                                                                                                       || FALSE
        '"funcky:lists".contains [0] 0'                                                                                                                                                                                      || TRUE
        '"funcky:lists".contains [0] 1'                                                                                                                                                                                      || FALSE
        '"funcky:lists".contains [0, 1] 0'                                                                                                                                                                                   || TRUE
        '"funcky:lists".contains [0, 1] 1'                                                                                                                                                                                   || TRUE
        '"funcky:lists".contains "" \'a\''                                                                                                                                                                                   || FALSE
        '"funcky:lists".contains "a" \'a\''                                                                                                                                                                                  || TRUE
        '"funcky:lists".contains "a" \'b\''                                                                                                                                                                                  || FALSE
        '"funcky:lists".contains "ab" \'a\''                                                                                                                                                                                 || TRUE
        '"funcky:lists".contains "ab" \'b\''                                                                                                                                                                                 || TRUE
        '"funcky:lists".contains [] ("funcky:commons".error "foo")'                                                                                                                                                          || FALSE
    }

    @Unroll('Test filter (expression: #expression)')
    def 'Test filter'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".filter)'                                                                                                                                                                   || FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".filter)))'                                                                                                               || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))'                                                                                         || TRUE
        '"funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".filter)))'                                                                                                                        || BOOLEAN
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))'                                                                                         || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".filter))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))' || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".filter))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))' || TRUE
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter [0])))'                                                                                                                                        || NUMBER
        '"funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter [0])))'                                                                                                                                         || BOOLEAN
        '"funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".filter [0])))'                                                                                                                                        || NUMBER
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter "a")))'                                                                                                                                        || CHARACTER
        '"funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter "a")))'                                                                                                                                         || BOOLEAN
        '"funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".filter "a")))'                                                                                                                                        || CHARACTER
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".filter ("funcky:commons".error "foo")))'                                                                                                                                  || FALSE
        '"funcky:lists".filter [] ("funcky:commons".lessEqual 0)'                                                                                                                                                                                || new FunckyJavaConverter().convert([])
        '"funcky:lists".filter [0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                               || new FunckyJavaConverter().convert([0.0G])
        '"funcky:lists".filter [1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                               || new FunckyJavaConverter().convert([1.0G])
        '"funcky:lists".filter [-1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                              || new FunckyJavaConverter().convert([])
        '"funcky:lists".filter [0, 0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || new FunckyJavaConverter().convert([0.0G, 0.0G])
        '"funcky:lists".filter [0, 1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || new FunckyJavaConverter().convert([0.0G, 1.0G])
        '"funcky:lists".filter [0, -1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || new FunckyJavaConverter().convert([0.0G])
        '"funcky:lists".filter [1, 0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || new FunckyJavaConverter().convert([1.0G, 0.0G])
        '"funcky:lists".filter [1, 1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || new FunckyJavaConverter().convert([1.0G, 1.0G])
        '"funcky:lists".filter [1, -1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || new FunckyJavaConverter().convert([1.0G])
        '"funcky:lists".filter [-1, 0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || new FunckyJavaConverter().convert([0.0G])
        '"funcky:lists".filter [-1, 1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || new FunckyJavaConverter().convert([1.0G])
        '"funcky:lists".filter [-1, -1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                          || new FunckyJavaConverter().convert([])
        '"funcky:lists".filter "" ("funcky:commons".equal \'a\')'                                                                                                                                                                                || new FunckyJavaConverter().convert('')
        '"funcky:lists".filter "a" ("funcky:commons".equal \'a\')'                                                                                                                                                                               || new FunckyJavaConverter().convert('a')
        '"funcky:lists".filter "b" ("funcky:commons".equal \'a\')'                                                                                                                                                                               || new FunckyJavaConverter().convert('')
        '"funcky:lists".filter "aa" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || new FunckyJavaConverter().convert('aa')
        '"funcky:lists".filter "ab" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || new FunckyJavaConverter().convert('a')
        '"funcky:lists".filter "ba" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || new FunckyJavaConverter().convert('a')
        '"funcky:lists".filter "bb" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || new FunckyJavaConverter().convert('')
        '"funcky:lists".filter [] ("funcky:commons".error "foo")'                                                                                                                                                                                || new FunckyJavaConverter().convert([])
    }

    @Unroll('Test map (expression: #expression)')
    def 'Test map'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                              || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".map)'                                                                                                                                                                                     || FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".map)))'                                                                                                                                 || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                                                                                                           || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                                                                                                            || TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                                                                                                           || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".map))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                      || TRUE
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map)))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".map))))' || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".map))) ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                       || FALSE
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".map [0])))'                                                                                                                                                          || NUMBER
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".map [0]))))'                                                                                                                             || TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".map [0]))))'                                                                                                                            || TRUE
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".map "a")))'                                                                                                                                                          || CHARACTER
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".map "a"))))'                                                                                                                             || TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".map "a"))))'                                                                                                                            || TRUE
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".map ("funcky:commons".error "foo")))'                                                                                                                                                    || FALSE
        '"funcky:lists".map [] ("funcky:numbers".add 1)'                                                                                                                                                                                                        || new FunckyJavaConverter().convert([])
        '"funcky:lists".map [0] ("funcky:numbers".add 1)'                                                                                                                                                                                                       || new FunckyJavaConverter().convert([1.0G])
        '"funcky:lists".map [0, 1] ("funcky:numbers".add 1)'                                                                                                                                                                                                    || new FunckyJavaConverter().convert([1.0G, 2.0G])
        '"funcky:lists".map "" "funcky:characters".uppercase'                                                                                                                                                                                                   || new FunckyJavaConverter().convert('')
        '"funcky:lists".map "a" "funcky:characters".uppercase'                                                                                                                                                                                                  || new FunckyJavaConverter().convert('A')
        '"funcky:lists".map "ab" "funcky:characters".uppercase'                                                                                                                                                                                                 || new FunckyJavaConverter().convert('AB')
        '"funcky:lists".map [] ("funcky:commons".error "foo")'                                                                                                                                                                                                  || new FunckyJavaConverter().convert([])
    }

    @Unroll('Test reduce (expression: #expression)')
    def 'Test reduce'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                                                                                  || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".reduce)'                                                                                                                                                                                                                                      || FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".reduce)))'                                                                                                                                                                                  || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                                                                                                            || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))'                                                                                                                                     || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))'                                                                                                                                      || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                                                                                                             || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                                                                                                              || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".reduce))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                    || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))' || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                        || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                         || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".reduce))) ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))'                                             || FALSE
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0])))'                                                                                                                                                                                                           || NUMBER
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0])))))'                                                                                                                                                      || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0])))))'                                                                                                                                                       || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce [0]))))'                                                                                                                                                                              || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce [0]))))'                                                                                                                                                                               || TRUE
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce "a")))'                                                                                                                                                                                                           || CHARACTER
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce "a")))))'                                                                                                                                                      || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce "a")))))'                                                                                                                                                       || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce "a"))))'                                                                                                                                                                              || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce "a"))))'                                                                                                                                                                               || TRUE
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0] "funcky:numbers".add))'                                                                                                                                                                                                              || NUMBER
        '"funcky:types".range ("funcky:types".type ("funcky:lists".reduce [0] "funcky:numbers".add))'                                                                                                                                                                                                               || NUMBER
        '"funcky:lists".reduce [] "funcky:numbers".add 0'                                                                                                                                                                                                                                                           || new FunckyNumber(0.0G)
        '"funcky:lists".reduce [1] "funcky:numbers".add 0'                                                                                                                                                                                                                                                          || new FunckyNumber(1.0G)
        '"funcky:lists".reduce [1, 2] "funcky:numbers".add 0'                                                                                                                                                                                                                                                       || new FunckyNumber(3.0G)
        '"funcky:lists".reduce [] ("funcky:commons".error "foo") 0'                                                                                                                                                                                                                                                 || new FunckyNumber(0.0G)
    }

    @Unroll('Test sort (expression: #expression)')
    def 'Test sort'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                    || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".sort)'                                                                                                                                          || FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sort)))'                                                                                      || TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".sort)))'                                                                                       || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sort))) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".sort)))' || TRUE
        '"funcky:lists".sort []'                                                                                                                                                                                      || new FunckyJavaConverter().convert([])
        '"funcky:lists".sort [0]'                                                                                                                                                                                     || new FunckyJavaConverter().convert([0.0G])
        '"funcky:lists".sort [0, 0]'                                                                                                                                                                                  || new FunckyJavaConverter().convert([0.0G, 0.0G])
        '"funcky:lists".sort [0, 1]'                                                                                                                                                                                  || new FunckyJavaConverter().convert([0.0G, 1.0G])
        '"funcky:lists".sort [1, 0]'                                                                                                                                                                                  || new FunckyJavaConverter().convert([0.0G, 1.0G])
        '"funcky:lists".sort [1, 1]'                                                                                                                                                                                  || new FunckyJavaConverter().convert([1.0G, 1.0G])
    }

    @Unroll('Test repeat (expression: #expression)')
    def 'Test repeat'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".repeat)'                                                                                                                   || FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:lists".repeat))'                                                                                        || TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".repeat)))'                                                                || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:lists".repeat)) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".repeat)))' || TRUE
        '"funcky:lists".empty ("funcky:lists".repeat 0)'                                                                                                                                         || FALSE
        '"funcky:lists".empty ("funcky:lists".repeat \'a\')'                                                                                                                                     || FALSE
        '"funcky:lists".empty ("funcky:lists".repeat ("funcky:commons".error "foo"))'                                                                                                            || FALSE
    }

    @Unroll('Test take (expression: #expression)')
    def 'Test take'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                           || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".take)'                                                                                                                                                                 || FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".take)))'                                                                                                             || TRUE
        '"funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".take))'                                                                                                                                             || NUMBER
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".take))))'                                                                                       || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".take))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".take))))' || TRUE
        '"funcky:types".type ("funcky:lists".take [0])'                                                                                                                                                                                      || FUNCTION(NUMBER, LIST(NUMBER))
        '"funcky:types".type ("funcky:lists".take "a")'                                                                                                                                                                                      || FUNCTION(NUMBER, STRING)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".take ("funcky:commons".error "foo")))'                                                                                                                                || FALSE
        '"funcky:lists".take [] 0'                                                                                                                                                                                                           || new FunckyJavaConverter().convert([])
        '"funcky:lists".take [] 1'                                                                                                                                                                                                           || new FunckyJavaConverter().convert([])
        '"funcky:lists".take [0] 0'                                                                                                                                                                                                          || new FunckyJavaConverter().convert([])
        '"funcky:lists".take [0] 1'                                                                                                                                                                                                          || new FunckyJavaConverter().convert([0.0G])
        '"funcky:lists".take [0] 2'                                                                                                                                                                                                          || new FunckyJavaConverter().convert([0.0G])
        '"funcky:lists".take [0, 1] 0'                                                                                                                                                                                                       || new FunckyJavaConverter().convert([])
        '"funcky:lists".take [0, 1] 1'                                                                                                                                                                                                       || new FunckyJavaConverter().convert([0.0G])
        '"funcky:lists".take [0, 1] 2'                                                                                                                                                                                                       || new FunckyJavaConverter().convert([0.0G, 1.0G])
        '"funcky:lists".take [0, 1] 3'                                                                                                                                                                                                       || new FunckyJavaConverter().convert([0.0, 1.0G])
        '"funcky:lists".take "" 0'                                                                                                                                                                                                           || new FunckyJavaConverter().convert('')
        '"funcky:lists".take "" 1'                                                                                                                                                                                                           || new FunckyJavaConverter().convert('')
        '"funcky:lists".take "a" 0'                                                                                                                                                                                                          || new FunckyJavaConverter().convert('')
        '"funcky:lists".take "a" 1'                                                                                                                                                                                                          || new FunckyJavaConverter().convert('a')
        '"funcky:lists".take "a" 2'                                                                                                                                                                                                          || new FunckyJavaConverter().convert('a')
        '"funcky:lists".take "ab" 0'                                                                                                                                                                                                         || new FunckyJavaConverter().convert('')
        '"funcky:lists".take "ab" 1'                                                                                                                                                                                                         || new FunckyJavaConverter().convert('a')
        '"funcky:lists".take "ab" 2'                                                                                                                                                                                                         || new FunckyJavaConverter().convert('ab')
        '"funcky:lists".take "ab" 3'                                                                                                                                                                                                         || new FunckyJavaConverter().convert('ab')
        '"funcky:lists".take ("funcky:commons".error "foo") 0'                                                                                                                                                                               || new FunckyJavaConverter().convert([])
    }

    @Unroll('Test take (runtime error, expression: #expression)')
    def 'Test take (runtime error)'(final String expression, final String error) {
        when:
        engine.eval(expression)
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(error)
        where:
        expression                   || error
        '"funcky:lists".take [] -1'  || 'Invalid n `-1`'
        '"funcky:lists".take "" 5.5' || 'Invalid n `5.5`'
    }

    @Unroll('Test drop (expression: #expression)')
    def 'Test drop'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                           || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".drop)'                                                                                                                                                                 || FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".drop)))'                                                                                                             || TRUE
        '"funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".drop))'                                                                                                                                             || NUMBER
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".drop))))'                                                                                       || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".drop))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".drop))))' || TRUE
        '"funcky:types".type ("funcky:lists".drop [0])'                                                                                                                                                                                      || FUNCTION(NUMBER, LIST(NUMBER))
        '"funcky:types".type ("funcky:lists".drop "a")'                                                                                                                                                                                      || FUNCTION(NUMBER, STRING)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".drop ("funcky:commons".error "foo")))'                                                                                                                                || FALSE
        '"funcky:lists".drop [] 0'                                                                                                                                                                                                           || new FunckyJavaConverter().convert([])
        '"funcky:lists".drop [] 1'                                                                                                                                                                                                           || new FunckyJavaConverter().convert([])
        '"funcky:lists".drop [0] 0'                                                                                                                                                                                                          || new FunckyJavaConverter().convert([0.0G])
        '"funcky:lists".drop [0] 1'                                                                                                                                                                                                          || new FunckyJavaConverter().convert([])
        '"funcky:lists".drop [0] 2'                                                                                                                                                                                                          || new FunckyJavaConverter().convert([])
        '"funcky:lists".drop [0, 1] 0'                                                                                                                                                                                                       || new FunckyJavaConverter().convert([0.0G, 1.0G])
        '"funcky:lists".drop [0, 1] 1'                                                                                                                                                                                                       || new FunckyJavaConverter().convert([1.0G])
        '"funcky:lists".drop [0, 1] 2'                                                                                                                                                                                                       || new FunckyJavaConverter().convert([])
        '"funcky:lists".drop [0, 1] 3'                                                                                                                                                                                                       || new FunckyJavaConverter().convert([])
        '"funcky:lists".drop "" 0'                                                                                                                                                                                                           || new FunckyJavaConverter().convert('')
        '"funcky:lists".drop "" 1'                                                                                                                                                                                                           || new FunckyJavaConverter().convert('')
        '"funcky:lists".drop "a" 0'                                                                                                                                                                                                          || new FunckyJavaConverter().convert('a')
        '"funcky:lists".drop "a" 1'                                                                                                                                                                                                          || new FunckyJavaConverter().convert('')
        '"funcky:lists".drop "a" 2'                                                                                                                                                                                                          || new FunckyJavaConverter().convert('')
        '"funcky:lists".drop "ab" 0'                                                                                                                                                                                                         || new FunckyJavaConverter().convert('ab')
        '"funcky:lists".drop "ab" 1'                                                                                                                                                                                                         || new FunckyJavaConverter().convert('b')
        '"funcky:lists".drop "ab" 2'                                                                                                                                                                                                         || new FunckyJavaConverter().convert('')
        '"funcky:lists".drop "ab" 3'                                                                                                                                                                                                         || new FunckyJavaConverter().convert('')
    }

    @Unroll('Test drop (runtime error, expression: #expression)')
    def 'Test drop (runtime error)'(final String expression, final String error) {
        when:
        engine.eval(expression)
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(error)
        where:
        expression                   || error
        '"funcky:lists".drop [] -1'  || 'Invalid n `-1`'
        '"funcky:lists".drop "" 5.5' || 'Invalid n `5.5`'
    }

    @Unroll('Test sublist (expression: #expression)')
    def 'Test sublist'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                                        || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".sublist)'                                                                                                                                                                                           || FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sublist)))'                                                                                                                                       || TRUE
        '"funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".sublist))'                                                                                                                                                                       || NUMBER
        '"funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".sublist)))'                                                                                                                                                || NUMBER
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".sublist)))))'                                                                                          || TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sublist))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".sublist)))))' || TRUE
        '"funcky:types".type ("funcky:lists".sublist [0])'                                                                                                                                                                                                                || FUNCTION(NUMBER, NUMBER, LIST(NUMBER))
        '"funcky:types".type ("funcky:lists".sublist "a")'                                                                                                                                                                                                                || FUNCTION(NUMBER, NUMBER, STRING)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".sublist ("funcky:commons".error "foo")))'                                                                                                                                                          || FALSE
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".sublist ("funcky:commons".error "foo") ("funcky:commons".error "foo")))'                                                                                                                           || FALSE
        '"funcky:lists".sublist [] 0 0'                                                                                                                                                                                                                                   || new FunckyJavaConverter().convert([])
        '"funcky:lists".sublist [0] 0 0'                                                                                                                                                                                                                                  || new FunckyJavaConverter().convert([])
        '"funcky:lists".sublist [0] 0 1'                                                                                                                                                                                                                                  || new FunckyJavaConverter().convert([0.0G])
        '"funcky:lists".sublist [0] 1 1'                                                                                                                                                                                                                                  || new FunckyJavaConverter().convert([])
        '"funcky:lists".sublist [0, 1] 0 0'                                                                                                                                                                                                                               || new FunckyJavaConverter().convert([])
        '"funcky:lists".sublist [0, 1] 0 1'                                                                                                                                                                                                                               || new FunckyJavaConverter().convert([0.0G])
        '"funcky:lists".sublist [0, 1] 0 2'                                                                                                                                                                                                                               || new FunckyJavaConverter().convert([0.0G, 1.0G])
        '"funcky:lists".sublist [0, 1] 1 1'                                                                                                                                                                                                                               || new FunckyJavaConverter().convert([])
        '"funcky:lists".sublist [0, 1] 1 2'                                                                                                                                                                                                                               || new FunckyJavaConverter().convert([1.0G])
        '"funcky:lists".sublist [0, 1] 2 2'                                                                                                                                                                                                                               || new FunckyJavaConverter().convert([])
    }

    @Unroll('Test sublist (runtime error, expression: #expression)')
    def 'Test sublist (runtime error)'(final String expression, final String error) {
        when:
        engine.eval(expression)
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(error)
        where:
        expression                         || error
        '"funcky:lists".sublist [0] -1 0'  || 'Invalid start `-1`, should be an int between `0` and `0`'
        '"funcky:lists".sublist [0] 0.5 1' || 'Invalid start `0.5`, should be an int between `0` and `1`'
        '"funcky:lists".sublist [0] 2 1'   || 'Invalid start `2`, should be an int between `0` and `1`'
        '"funcky:lists".sublist [0] 0 -1'  || 'Invalid end `-1`, should be an int between `0` and `1`'
        '"funcky:lists".sublist [0] 0 0.5' || 'Invalid end `0.5`, should be an int between `0` and `1`'
        '"funcky:lists".sublist [0] 0 2'   || 'Invalid end `2`, should be an int between `0` and `1`'
    }
}
