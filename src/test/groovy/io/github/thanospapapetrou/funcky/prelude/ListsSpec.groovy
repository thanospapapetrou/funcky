package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException
import io.github.thanospapapetrou.funcky.runtime.prelude.Lists
import spock.lang.Unroll

import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.FALSE
import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.TRUE
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType.FUNCTION
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.LIST
import static io.github.thanospapapetrou.funcky.runtime.types.FunckyListType.STRING
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.BOOLEAN
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.CHARACTER
import static io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType.NUMBER

class ListsSpec extends BaseSpec {
    @Unroll('Test head (expression: #expression)')
    def 'Test head'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                           || result
        '"funcky:lists".head'                                                                                                                                                                || new Lists(engine).$head
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".head)))'                                                             || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".type "funcky:lists".head))'                                                                                       || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".head))) ("funcky:types".range ("funcky:types".type "funcky:lists".head))' || TRUE.apply(engine)
        '"funcky:lists".head [0]'                                                                                                                                                            || new FunckyNumber(engine, 0.0G)
        '"funcky:lists".head [0, 1]'                                                                                                                                                         || new FunckyNumber(engine, 0.0G)
        '"funcky:lists".head "a"'                                                                                                                                                            || new FunckyCharacter(engine, 'a' as char)
        '"funcky:lists".head "ab"'                                                                                                                                                           || new FunckyCharacter(engine, 'a' as char)
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
        '"funcky:lists".tail'                                                                                                                                                                                         || new Lists(engine).$tail
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".tail)))'                                                                                      || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".tail)))'                                                                                       || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".tail))) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".tail)))' || TRUE.apply(engine)
        '"funcky:lists".tail [0]'                                                                                                                                                                                     || toFuncky([])
        '"funcky:lists".tail [0, 1]'                                                                                                                                                                                  || toFuncky([1.0G])
        '"funcky:lists".tail "a"'                                                                                                                                                                                     || toFuncky('')
        '"funcky:lists".tail "ab"'                                                                                                                                                                                    || toFuncky('b')
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
        '"funcky:lists".prepend'                                                                                                                                                                                                                   || new Lists(engine).$prepend
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".prepend)))'                                                                                                                || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".prepend)))'                                                                                                                  || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".prepend))))'                                                                                          || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".prepend))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".prepend)))'                         || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".prepend))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".prepend))))' || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type ("funcky:lists".prepend [])))'                                                                                                                                    || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".prepend []))))'                                                                                                            || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type ("funcky:lists".prepend []))) ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".prepend []))))'                                       || FALSE.apply(engine)
        '"funcky:types".type ("funcky:lists".prepend [0])'                                                                                                                                                                                         || FUNCTION(NUMBER, LIST(NUMBER)).apply(engine)
        '"funcky:types".type ("funcky:lists".prepend "")'                                                                                                                                                                                          || FUNCTION(CHARACTER, STRING).apply(engine)
        '"funcky:commons".string ("funcky:lists".prepend ("funcky:commons".error "foo"))'                                                                                                                                                          || toFuncky('"funcky:lists".prepend ("funcky:commons".error "foo")')
        '"funcky:lists".prepend [] 0'                                                                                                                                                                                                              || toFuncky([0.0G])
        '"funcky:lists".prepend [1] 0'                                                                                                                                                                                                             || toFuncky([0.0G, 1.0G])
        '"funcky:lists".prepend "" \'a\''                                                                                                                                                                                                          || toFuncky('a')
        '"funcky:lists".prepend "b" \'a\''                                                                                                                                                                                                         || toFuncky('ab')
    }

    @Unroll('Test append (expression: #expression)')
    def 'Test append'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".append)'                                                                                                                                                                   || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".append)))'                                                                                                               || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".append)))'                                                                                                                 || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".append))))'                                                                                         || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".append))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".append)))'                         || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".append))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".append))))' || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type ("funcky:lists".append [])))'                                                                                                                                   || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".append []))))'                                                                                                           || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type ("funcky:lists".append []))) ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".append []))))'                                       || FALSE.apply(engine)
        '"funcky:types".type ("funcky:lists".append [0])'                                                                                                                                                                                        || FUNCTION(NUMBER, LIST(NUMBER)).apply(engine)
        '"funcky:types".type ("funcky:lists".append "")'                                                                                                                                                                                         || FUNCTION(CHARACTER, STRING).apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".append ("funcky:commons".error "foo")))'                                                                                                                                  || FALSE.apply(engine)
        '"funcky:lists".append [] 0'                                                                                                                                                                                                             || toFuncky([0.0G])
        '"funcky:lists".append [1] 0'                                                                                                                                                                                                            || toFuncky([1.0G, 0.0G])
        '"funcky:lists".append "" \'a\''                                                                                                                                                                                                         || toFuncky('a')
        '"funcky:lists".append "b" \'a\''                                                                                                                                                                                                        || toFuncky('ba')
    }

    @Unroll('Test concat (expression: #expression)')
    def 'Test concat'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".concat)'                                                                                                                                                                    || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".concat)))'                                                                                                                || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))'                                                                                         || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))'                                                                                          || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".concat))) ("funcky:types".element ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))' || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".concat))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))'  || TRUE.apply(engine)
        '"funcky:types".type ("funcky:lists".concat [0])'                                                                                                                                                                                         || FUNCTION(LIST(NUMBER), LIST(NUMBER)).apply(engine)
        '"funcky:types".type ("funcky:lists".concat "")'                                                                                                                                                                                          || FUNCTION(STRING, STRING).apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".concat ("funcky:commons".error "foo")))'                                                                                                                                   || FALSE.apply(engine)
        '"funcky:lists".concat [] []'                                                                                                                                                                                                             || toFuncky([])
        '"funcky:lists".concat [] [0]'                                                                                                                                                                                                            || toFuncky([0.0G])
        '"funcky:lists".concat [0] []'                                                                                                                                                                                                            || toFuncky([0.0G])
        '"funcky:lists".concat [0] [1]'                                                                                                                                                                                                           || toFuncky([0.0G, 1.0G])
        '"funcky:lists".concat "" ""'                                                                                                                                                                                                             || toFuncky('')
        '"funcky:lists".concat "" "a"'                                                                                                                                                                                                            || toFuncky('a')
        '"funcky:lists".concat "a" ""'                                                                                                                                                                                                            || toFuncky('a')
        '"funcky:lists".concat "a" "b"'                                                                                                                                                                                                           || toFuncky('ab')
    }

    @Unroll('Test singleton (expression: #expression)')
    def 'Test singleton'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                     || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".singleton)'                                                                                                                      || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:lists".singleton))'                                                                                           || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".singleton)))'                                                                   || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:lists".singleton)) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".singleton)))' || TRUE.apply(engine)
        '"funcky:lists".singleton 0'                                                                                                                                                                   || toFuncky([0.0G])
        '"funcky:lists".singleton \'a\''                                                                                                                                                               || toFuncky('a')
    }

    @Unroll('Test empty (expression: #expression)')
    def 'Test empty'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                || result
        '"funcky:commons".string "funcky:lists".empty'                                                                            || toFuncky('"funcky:commons".equal []')
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".empty)))' || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".type "funcky:lists".empty)'                                                         || BOOLEAN.apply(engine)
        '"funcky:lists".empty []'                                                                                                 || TRUE.apply(engine)
        '"funcky:lists".empty [0]'                                                                                                || FALSE.apply(engine)
        '"funcky:lists".empty ""'                                                                                                 || TRUE.apply(engine)
        '"funcky:lists".empty "a"'                                                                                                || FALSE.apply(engine)
    }

    @Unroll('Test size (expression: #expression)')
    def 'Test size'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".size)'                                                     || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".size)))' || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".type "funcky:lists".size)'                                                         || NUMBER.apply(engine)
        '"funcky:lists".size []'                                                                                                 || new FunckyNumber(engine, 0.0G)
        '"funcky:lists".size [0]'                                                                                                || new FunckyNumber(engine, 1.0G)
        '"funcky:lists".size ""'                                                                                                 || new FunckyNumber(engine, 0.0G)
        '"funcky:lists".size "a"'                                                                                                || new FunckyNumber(engine, 1.0G)
    }

    @Unroll('Test index (expression: #expression)')
    def 'Test index'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                     || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".index)'                                                                                                                                          || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".index)))'                                                                                      || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".index)))'                                                                                        || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".index))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".index)))' || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".index))'                                                                                                                       || NUMBER.apply(engine)
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".index [0]))'                                                                                                                                       || NUMBER.apply(engine)
        '"funcky:types".range ("funcky:types".type ("funcky:lists".index [0]))'                                                                                                                                        || NUMBER.apply(engine)
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".index "a"))'                                                                                                                                       || CHARACTER.apply(engine)
        '"funcky:types".range ("funcky:types".type ("funcky:lists".index "a"))'                                                                                                                                        || NUMBER.apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".index ("funcky:commons".error "foo")))'                                                                                                         || FALSE.apply(engine)
        '"funcky:lists".index [] 0'                                                                                                                                                                                    || new FunckyNumber(engine, -1.0G)
        '"funcky:lists".index [0] 0'                                                                                                                                                                                   || new FunckyNumber(engine, 0.0G)
        '"funcky:lists".index [0] 1'                                                                                                                                                                                   || new FunckyNumber(engine, -1.0G)
        '"funcky:lists".index [0, 1] 0'                                                                                                                                                                                || new FunckyNumber(engine, 0.0G)
        '"funcky:lists".index [0, 1] 1'                                                                                                                                                                                || new FunckyNumber(engine, 1.0G)
        '"funcky:lists".index "" \'a\''                                                                                                                                                                                || new FunckyNumber(engine, -1.0G)
        '"funcky:lists".index "a" \'a\''                                                                                                                                                                               || new FunckyNumber(engine, 0.0G)
        '"funcky:lists".index "a" \'b\''                                                                                                                                                                               || new FunckyNumber(engine, -1.0G)
        '"funcky:lists".index "ab" \'a\''                                                                                                                                                                              || new FunckyNumber(engine, 0.0G)
        '"funcky:lists".index "ab" \'b\''                                                                                                                                                                              || new FunckyNumber(engine, 1.0G)
        '"funcky:lists".index [] ("funcky:commons".error "foo")'                                                                                                                                                       || new FunckyNumber(engine, -1.0G)
    }

    @Unroll('Test contains (expression: #expression)')
    def 'Test contains'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                           || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".contains)'                                                                                                                                             || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".contains)))'                                                                                         || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".contains)))'                                                                                           || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".contains))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".contains)))' || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".contains))'                                                                                                                          || BOOLEAN.apply(engine)
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".contains [0]))'                                                                                                                                          || NUMBER.apply(engine)
        '"funcky:types".range ("funcky:types".type ("funcky:lists".contains [0]))'                                                                                                                                           || BOOLEAN.apply(engine)
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".contains "a"))'                                                                                                                                          || CHARACTER.apply(engine)
        '"funcky:types".range ("funcky:types".type ("funcky:lists".contains "a"))'                                                                                                                                           || BOOLEAN.apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".contains ("funcky:commons".error "foo")))'                                                                                                            || FALSE.apply(engine)
        '"funcky:lists".contains [] 0'                                                                                                                                                                                       || FALSE.apply(engine)
        '"funcky:lists".contains [0] 0'                                                                                                                                                                                      || TRUE.apply(engine)
        '"funcky:lists".contains [0] 1'                                                                                                                                                                                      || FALSE.apply(engine)
        '"funcky:lists".contains [0, 1] 0'                                                                                                                                                                                   || TRUE.apply(engine)
        '"funcky:lists".contains [0, 1] 1'                                                                                                                                                                                   || TRUE.apply(engine)
        '"funcky:lists".contains "" \'a\''                                                                                                                                                                                   || FALSE.apply(engine)
        '"funcky:lists".contains "a" \'a\''                                                                                                                                                                                  || TRUE.apply(engine)
        '"funcky:lists".contains "a" \'b\''                                                                                                                                                                                  || FALSE.apply(engine)
        '"funcky:lists".contains "ab" \'a\''                                                                                                                                                                                 || TRUE.apply(engine)
        '"funcky:lists".contains "ab" \'b\''                                                                                                                                                                                 || TRUE.apply(engine)
        '"funcky:lists".contains [] ("funcky:commons".error "foo")'                                                                                                                                                          || FALSE.apply(engine)
    }

    @Unroll('Test filter (expression: #expression)')
    def 'Test filter'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".filter)'                                                                                                                                                                   || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".filter)))'                                                                                                               || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))'                                                                                         || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".filter)))'                                                                                                                        || BOOLEAN.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))'                                                                                         || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".filter))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))' || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".filter))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))' || TRUE.apply(engine)
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter [0])))'                                                                                                                                        || NUMBER.apply(engine)
        '"funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter [0])))'                                                                                                                                         || BOOLEAN.apply(engine)
        '"funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".filter [0])))'                                                                                                                                        || NUMBER.apply(engine)
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter "a")))'                                                                                                                                        || CHARACTER.apply(engine)
        '"funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter "a")))'                                                                                                                                         || BOOLEAN.apply(engine)
        '"funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".filter "a")))'                                                                                                                                        || CHARACTER.apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".filter ("funcky:commons".error "foo")))'                                                                                                                                  || FALSE.apply(engine)
        '"funcky:lists".filter [] ("funcky:commons".lessEqual 0)'                                                                                                                                                                                || toFuncky([])
        '"funcky:lists".filter [0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                               || toFuncky([0.0G])
        '"funcky:lists".filter [1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                               || toFuncky([1.0G])
        '"funcky:lists".filter [-1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                              || toFuncky([])
        '"funcky:lists".filter [0, 0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || toFuncky([0.0G, 0.0G])
        '"funcky:lists".filter [0, 1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || toFuncky([0.0G, 1.0G])
        '"funcky:lists".filter [0, -1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || toFuncky([0.0G])
        '"funcky:lists".filter [1, 0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || toFuncky([1.0G, 0.0G])
        '"funcky:lists".filter [1, 1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || toFuncky([1.0G, 1.0G])
        '"funcky:lists".filter [1, -1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || toFuncky([1.0G])
        '"funcky:lists".filter [-1, 0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || toFuncky([0.0G])
        '"funcky:lists".filter [-1, 1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || toFuncky([1.0G])
        '"funcky:lists".filter [-1, -1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                          || toFuncky([])
        '"funcky:lists".filter "" ("funcky:commons".equal \'a\')'                                                                                                                                                                                || toFuncky('')
        '"funcky:lists".filter "a" ("funcky:commons".equal \'a\')'                                                                                                                                                                               || toFuncky('a')
        '"funcky:lists".filter "b" ("funcky:commons".equal \'a\')'                                                                                                                                                                               || toFuncky('')
        '"funcky:lists".filter "aa" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || toFuncky('aa')
        '"funcky:lists".filter "ab" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || toFuncky('a')
        '"funcky:lists".filter "ba" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || toFuncky('a')
        '"funcky:lists".filter "bb" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || toFuncky('')
        '"funcky:lists".filter [] ("funcky:commons".error "foo")'                                                                                                                                                                                || toFuncky([])
    }

    @Unroll('Test map (expression: #expression)')
    def 'Test map'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                              || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".map)'                                                                                                                                                                                     || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".map)))'                                                                                                                                 || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                                                                                                           || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                                                                                                            || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                                                                                                           || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".map))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                      || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map)))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".map))))' || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".map))) ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                       || FALSE.apply(engine)
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".map [0])))'                                                                                                                                                          || NUMBER.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".map [0]))))'                                                                                                                             || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".map [0]))))'                                                                                                                            || TRUE.apply(engine)
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".map "a")))'                                                                                                                                                          || CHARACTER.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".map "a"))))'                                                                                                                             || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".map "a"))))'                                                                                                                            || TRUE.apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".map ("funcky:commons".error "foo")))'                                                                                                                                                    || FALSE.apply(engine)
        '"funcky:lists".map [] ("funcky:numbers".add 1)'                                                                                                                                                                                                        || toFuncky([])
        '"funcky:lists".map [0] ("funcky:numbers".add 1)'                                                                                                                                                                                                       || toFuncky([1.0G])
        '"funcky:lists".map [0, 1] ("funcky:numbers".add 1)'                                                                                                                                                                                                    || toFuncky([1.0G, 2.0G])
        '"funcky:lists".map "" "funcky:characters".uppercase'                                                                                                                                                                                                   || toFuncky('')
        '"funcky:lists".map "a" "funcky:characters".uppercase'                                                                                                                                                                                                  || toFuncky('A')
        '"funcky:lists".map "ab" "funcky:characters".uppercase'                                                                                                                                                                                                 || toFuncky('AB')
        '"funcky:lists".map [] ("funcky:commons".error "foo")'                                                                                                                                                                                                  || toFuncky([])
    }

    @Unroll('Test reduce (expression: #expression)')
    def 'Test reduce'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                                                                                  || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".reduce)'                                                                                                                                                                                                                                      || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".reduce)))'                                                                                                                                                                                  || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                                                                                                            || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))'                                                                                                                                     || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))'                                                                                                                                      || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                                                                                                             || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                                                                                                              || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".reduce))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                    || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))' || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                        || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                         || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".reduce))) ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))'                                             || FALSE.apply(engine)
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0])))'                                                                                                                                                                                                           || NUMBER.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0])))))'                                                                                                                                                      || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0])))))'                                                                                                                                                       || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce [0]))))'                                                                                                                                                                              || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce [0]))))'                                                                                                                                                                               || TRUE.apply(engine)
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce "a")))'                                                                                                                                                                                                           || CHARACTER.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce "a")))))'                                                                                                                                                      || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce "a")))))'                                                                                                                                                       || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce "a"))))'                                                                                                                                                                              || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce "a"))))'                                                                                                                                                                               || TRUE.apply(engine)
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0] "funcky:numbers".add))'                                                                                                                                                                                                              || NUMBER.apply(engine)
        '"funcky:types".range ("funcky:types".type ("funcky:lists".reduce [0] "funcky:numbers".add))'                                                                                                                                                                                                               || NUMBER.apply(engine)
        '"funcky:lists".reduce [] "funcky:numbers".add 0'                                                                                                                                                                                                                                                           || new FunckyNumber(engine, 0.0G)
        '"funcky:lists".reduce [1] "funcky:numbers".add 0'                                                                                                                                                                                                                                                          || new FunckyNumber(engine, 1.0G)
        '"funcky:lists".reduce [1, 2] "funcky:numbers".add 0'                                                                                                                                                                                                                                                       || new FunckyNumber(engine, 3.0G)
        '"funcky:lists".reduce [] ("funcky:commons".error "foo") 0'                                                                                                                                                                                                                                                 || new FunckyNumber(engine, 0.0G)
    }

    @Unroll('Test sort (expression: #expression)')
    def 'Test sort'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                    || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".sort)'                                                                                                                                          || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sort)))'                                                                                      || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".sort)))'                                                                                       || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sort))) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".sort)))' || TRUE.apply(engine)
        '"funcky:lists".sort []'                                                                                                                                                                                      || toFuncky([])
        '"funcky:lists".sort [0]'                                                                                                                                                                                     || toFuncky([0.0G])
        '"funcky:lists".sort [0, 0]'                                                                                                                                                                                  || toFuncky([0.0G, 0.0G])
        '"funcky:lists".sort [0, 1]'                                                                                                                                                                                  || toFuncky([0.0G, 1.0G])
        '"funcky:lists".sort [1, 0]'                                                                                                                                                                                  || toFuncky([0.0G, 1.0G])
        '"funcky:lists".sort [1, 1]'                                                                                                                                                                                  || toFuncky([1.0G, 1.0G])
    }

    @Unroll('Test repeat (expression: #expression)')
    def 'Test repeat'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".repeat)'                                                                                                                   || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:lists".repeat))'                                                                                        || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".repeat)))'                                                                || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:lists".repeat)) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".repeat)))' || TRUE.apply(engine)
        '"funcky:lists".empty ("funcky:lists".repeat 0)'                                                                                                                                         || FALSE.apply(engine)
        '"funcky:lists".empty ("funcky:lists".repeat \'a\')'                                                                                                                                     || FALSE.apply(engine)
        '"funcky:lists".empty ("funcky:lists".repeat ("funcky:commons".error "foo"))'                                                                                                            || FALSE.apply(engine)
    }

    @Unroll('Test take (expression: #expression)')
    def 'Test take'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                           || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".take)'                                                                                                                                                                 || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".take)))'                                                                                                             || TRUE.apply(engine)
        '"funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".take))'                                                                                                                                             || NUMBER.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".take))))'                                                                                       || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".take))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".take))))' || TRUE.apply(engine)
        '"funcky:types".type ("funcky:lists".take [0])'                                                                                                                                                                                      || FUNCTION(NUMBER, LIST(NUMBER)).apply(engine)
        '"funcky:types".type ("funcky:lists".take "a")'                                                                                                                                                                                      || FUNCTION(NUMBER, STRING).apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".take ("funcky:commons".error "foo")))'                                                                                                                                || FALSE.apply(engine)
        '"funcky:lists".take [] 0'                                                                                                                                                                                                           || toFuncky([])
        '"funcky:lists".take [] 1'                                                                                                                                                                                                           || toFuncky([])
        '"funcky:lists".take [0] 0'                                                                                                                                                                                                          || toFuncky([])
        '"funcky:lists".take [0] 1'                                                                                                                                                                                                          || toFuncky([0.0G])
        '"funcky:lists".take [0] 2'                                                                                                                                                                                                          || toFuncky([0.0G])
        '"funcky:lists".take [0, 1] 0'                                                                                                                                                                                                       || toFuncky([])
        '"funcky:lists".take [0, 1] 1'                                                                                                                                                                                                       || toFuncky([0.0G])
        '"funcky:lists".take [0, 1] 2'                                                                                                                                                                                                       || toFuncky([0.0G, 1.0G])
        '"funcky:lists".take [0, 1] 3'                                                                                                                                                                                                       || toFuncky([0.0, 1.0G])
        '"funcky:lists".take "" 0'                                                                                                                                                                                                           || toFuncky('')
        '"funcky:lists".take "" 1'                                                                                                                                                                                                           || toFuncky('')
        '"funcky:lists".take "a" 0'                                                                                                                                                                                                          || toFuncky('')
        '"funcky:lists".take "a" 1'                                                                                                                                                                                                          || toFuncky('a')
        '"funcky:lists".take "a" 2'                                                                                                                                                                                                          || toFuncky('a')
        '"funcky:lists".take "ab" 0'                                                                                                                                                                                                         || toFuncky('')
        '"funcky:lists".take "ab" 1'                                                                                                                                                                                                         || toFuncky('a')
        '"funcky:lists".take "ab" 2'                                                                                                                                                                                                         || toFuncky('ab')
        '"funcky:lists".take "ab" 3'                                                                                                                                                                                                         || toFuncky('ab')
        '"funcky:lists".take ("funcky:commons".error "foo") 0'                                                                                                                                                                               || toFuncky([])
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
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".drop)'                                                                                                                                                                 || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".drop)))'                                                                                                             || TRUE.apply(engine)
        '"funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".drop))'                                                                                                                                             || NUMBER.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".drop))))'                                                                                       || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".drop))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".drop))))' || TRUE.apply(engine)
        '"funcky:types".type ("funcky:lists".drop [0])'                                                                                                                                                                                      || FUNCTION(NUMBER, LIST(NUMBER)).apply(engine)
        '"funcky:types".type ("funcky:lists".drop "a")'                                                                                                                                                                                      || FUNCTION(NUMBER, STRING).apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".drop ("funcky:commons".error "foo")))'                                                                                                                                || FALSE.apply(engine)
        '"funcky:lists".drop [] 0'                                                                                                                                                                                                           || toFuncky([])
        '"funcky:lists".drop [] 1'                                                                                                                                                                                                           || toFuncky([])
        '"funcky:lists".drop [0] 0'                                                                                                                                                                                                          || toFuncky([0.0G])
        '"funcky:lists".drop [0] 1'                                                                                                                                                                                                          || toFuncky([])
        '"funcky:lists".drop [0] 2'                                                                                                                                                                                                          || toFuncky([])
        '"funcky:lists".drop [0, 1] 0'                                                                                                                                                                                                       || toFuncky([0.0G, 1.0G])
        '"funcky:lists".drop [0, 1] 1'                                                                                                                                                                                                       || toFuncky([1.0G])
        '"funcky:lists".drop [0, 1] 2'                                                                                                                                                                                                       || toFuncky([])
        '"funcky:lists".drop [0, 1] 3'                                                                                                                                                                                                       || toFuncky([])
        '"funcky:lists".drop "" 0'                                                                                                                                                                                                           || toFuncky('')
        '"funcky:lists".drop "" 1'                                                                                                                                                                                                           || toFuncky('')
        '"funcky:lists".drop "a" 0'                                                                                                                                                                                                          || toFuncky('a')
        '"funcky:lists".drop "a" 1'                                                                                                                                                                                                          || toFuncky('')
        '"funcky:lists".drop "a" 2'                                                                                                                                                                                                          || toFuncky('')
        '"funcky:lists".drop "ab" 0'                                                                                                                                                                                                         || toFuncky('ab')
        '"funcky:lists".drop "ab" 1'                                                                                                                                                                                                         || toFuncky('b')
        '"funcky:lists".drop "ab" 2'                                                                                                                                                                                                         || toFuncky('')
        '"funcky:lists".drop "ab" 3'                                                                                                                                                                                                         || toFuncky('')
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
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".sublist)'                                                                                                                                                                                           || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sublist)))'                                                                                                                                       || TRUE.apply(engine)
        '"funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".sublist))'                                                                                                                                                                       || NUMBER.apply(engine)
        '"funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".sublist)))'                                                                                                                                                || NUMBER.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".sublist)))))'                                                                                          || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sublist))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".sublist)))))' || TRUE.apply(engine)
        '"funcky:types".type ("funcky:lists".sublist [0])'                                                                                                                                                                                                                || FUNCTION(NUMBER, NUMBER, LIST(NUMBER)).apply(engine)
        '"funcky:types".type ("funcky:lists".sublist "a")'                                                                                                                                                                                                                || FUNCTION(NUMBER, NUMBER, STRING).apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".sublist ("funcky:commons".error "foo")))'                                                                                                                                                          || FALSE.apply(engine)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".sublist ("funcky:commons".error "foo") ("funcky:commons".error "foo")))'                                                                                                                           || FALSE.apply(engine)
        '"funcky:lists".sublist [] 0 0'                                                                                                                                                                                                                                   || toFuncky([])
        '"funcky:lists".sublist [0] 0 0'                                                                                                                                                                                                                                  || toFuncky([])
        '"funcky:lists".sublist [0] 0 1'                                                                                                                                                                                                                                  || toFuncky([0.0G])
        '"funcky:lists".sublist [0] 1 1'                                                                                                                                                                                                                                  || toFuncky([])
        '"funcky:lists".sublist [0, 1] 0 0'                                                                                                                                                                                                                               || toFuncky([])
        '"funcky:lists".sublist [0, 1] 0 1'                                                                                                                                                                                                                               || toFuncky([0.0G])
        '"funcky:lists".sublist [0, 1] 0 2'                                                                                                                                                                                                                               || toFuncky([0.0G, 1.0G])
        '"funcky:lists".sublist [0, 1] 1 1'                                                                                                                                                                                                                               || toFuncky([])
        '"funcky:lists".sublist [0, 1] 1 2'                                                                                                                                                                                                                               || toFuncky([1.0G])
        '"funcky:lists".sublist [0, 1] 2 2'                                                                                                                                                                                                                               || toFuncky([])
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
