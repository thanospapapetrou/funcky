package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException
import io.github.thanospapapetrou.funcky.runtime.prelude.Lists
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType
import io.github.thanospapapetrou.funcky.runtime.FunckyListType
import spock.lang.Unroll

class ListsSpec extends BaseSpec {
    @Unroll('Test head (expression: #expression)')
    def 'Test head'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                           || result
        '"funcky:lists".head'                                                                                                                                                                || new Lists(engine).$head
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".head)))'                                                             || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".type "funcky:lists".head))'                                                                                       || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".head))) ("funcky:types".range ("funcky:types".type "funcky:lists".head))' || $true
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
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".tail)))'                                                                                      || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".tail)))'                                                                                       || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".tail))) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".tail)))' || $true
        '"funcky:lists".tail [0]'                                                                                                                                                                                     || engine.converter.convert(List.of())
        '"funcky:lists".tail [0, 1]'                                                                                                                                                                                  || engine.converter.convert(List.of(1.0G))
        '"funcky:lists".tail "a"'                                                                                                                                                                                     || engine.converter.convert('')
        '"funcky:lists".tail "ab"'                                                                                                                                                                                    || engine.converter.convert('b')
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
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".prepend)))'                                                                                                                || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".prepend)))'                                                                                                                  || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".prepend))))'                                                                                          || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".prepend))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".prepend)))'                         || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".prepend))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".prepend))))' || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type ("funcky:lists".prepend [])))'                                                                                                                                    || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".prepend []))))'                                                                                                            || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type ("funcky:lists".prepend []))) ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".prepend []))))'                                       || $false
        '"funcky:types".type ("funcky:lists".prepend [0])'                                                                                                                                                                                         || new FunckyFunctionType(engine, $Number, new FunckyListType(engine, $Number))
        '"funcky:types".type ("funcky:lists".prepend "")'                                                                                                                                                                                          || new FunckyFunctionType(engine, $Character, $String)
        '"funcky:commons".string ("funcky:lists".prepend ("funcky:commons".error "foo"))'                                                                                                                                                          || engine.converter.convert('"funcky:lists".prepend ("funcky:commons".error "foo")')
        '"funcky:lists".prepend [] 0'                                                                                                                                                                                                              || engine.converter.convert(List.of(0.0G))
        '"funcky:lists".prepend [1] 0'                                                                                                                                                                                                             || engine.converter.convert(List.of(0.0G, 1.0G))
        '"funcky:lists".prepend "" \'a\''                                                                                                                                                                                                          || engine.converter.convert('a')
        '"funcky:lists".prepend "b" \'a\''                                                                                                                                                                                                         || engine.converter.convert('ab')
    }

    @Unroll('Test append (expression: #expression)')
    def 'Test append'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".append)'                                                                                                                                                                   || $false
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".append)))'                                                                                                               || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".append)))'                                                                                                                 || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".append))))'                                                                                         || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".append))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".append)))'                         || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".append))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".append))))' || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type ("funcky:lists".append [])))'                                                                                                                                   || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".append []))))'                                                                                                           || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type ("funcky:lists".append []))) ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".append []))))'                                       || $false
        '"funcky:types".type ("funcky:lists".append [0])'                                                                                                                                                                                        || new FunckyFunctionType(engine, $Number, new FunckyListType(engine, $Number))
        '"funcky:types".type ("funcky:lists".append "")'                                                                                                                                                                                         || new FunckyFunctionType(engine, $Character, $String)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".append ("funcky:commons".error "foo")))'                                                                                                                                  || $false
        '"funcky:lists".append [] 0'                                                                                                                                                                                                             || engine.converter.convert(List.of(0.0G))
        '"funcky:lists".append [1] 0'                                                                                                                                                                                                            || engine.converter.convert(List.of(1.0G, 0.0G))
        '"funcky:lists".append "" \'a\''                                                                                                                                                                                                         || engine.converter.convert('a')
        '"funcky:lists".append "b" \'a\''                                                                                                                                                                                                        || engine.converter.convert('ba')
    }

    @Unroll('Test concat (expression: #expression)')
    def 'Test concat'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".concat)'                                                                                                                                                                    || $false
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".concat)))'                                                                                                                || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))'                                                                                         || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))'                                                                                          || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".concat))) ("funcky:types".element ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))' || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".concat))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))'  || $true
        '"funcky:types".type ("funcky:lists".concat [0])'                                                                                                                                                                                         || new FunckyFunctionType(engine, new FunckyListType(engine, $Number), new FunckyListType(engine, $Number))
        '"funcky:types".type ("funcky:lists".concat "")'                                                                                                                                                                                          || new FunckyFunctionType(engine, $String, $String)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".concat ("funcky:commons".error "foo")))'                                                                                                                                   || $false
        '"funcky:lists".concat [] []'                                                                                                                                                                                                             || engine.converter.convert(List.of())
        '"funcky:lists".concat [] [0]'                                                                                                                                                                                                            || engine.converter.convert(List.of(0.0G))
        '"funcky:lists".concat [0] []'                                                                                                                                                                                                            || engine.converter.convert(List.of(0.0G))
        '"funcky:lists".concat [0] [1]'                                                                                                                                                                                                           || engine.converter.convert(List.of(0.0G, 1.0G))
        '"funcky:lists".concat "" ""'                                                                                                                                                                                                             || engine.converter.convert('')
        '"funcky:lists".concat "" "a"'                                                                                                                                                                                                            || engine.converter.convert('a')
        '"funcky:lists".concat "a" ""'                                                                                                                                                                                                            || engine.converter.convert('a')
        '"funcky:lists".concat "a" "b"'                                                                                                                                                                                                           || engine.converter.convert('ab')
    }

    @Unroll('Test singleton (expression: #expression)')
    def 'Test singleton'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                     || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".singleton)'                                                                                                                      || $false
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:lists".singleton))'                                                                                           || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".singleton)))'                                                                   || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:lists".singleton)) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".singleton)))' || $true
        '"funcky:lists".singleton 0'                                                                                                                                                                   || engine.converter.convert(List.of(0.0G))
        '"funcky:lists".singleton \'a\''                                                                                                                                                               || engine.converter.convert('a')
    }

    @Unroll('Test empty (expression: #expression)')
    def 'Test empty'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                || result
        '"funcky:commons".string "funcky:lists".empty'                                                                            || engine.converter.convert('"funcky:commons".equal []')
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".empty)))' || $true
        '"funcky:types".range ("funcky:types".type "funcky:lists".empty)'                                                         || $Boolean
        '"funcky:lists".empty []'                                                                                                 || $true
        '"funcky:lists".empty [0]'                                                                                                || $false
        '"funcky:lists".empty ""'                                                                                                 || $true
        '"funcky:lists".empty "a"'                                                                                                || $false
    }

    @Unroll('Test size (expression: #expression)')
    def 'Test size'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".size)'                                                     || $false
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".size)))' || $true
        '"funcky:types".range ("funcky:types".type "funcky:lists".size)'                                                         || $Number
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
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".index)'                                                                                                                                          || $false
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".index)))'                                                                                      || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".index)))'                                                                                        || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".index))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".index)))' || $true
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".index))'                                                                                                                       || $Number
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".index [0]))'                                                                                                                                       || $Number
        '"funcky:types".range ("funcky:types".type ("funcky:lists".index [0]))'                                                                                                                                        || $Number
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".index "a"))'                                                                                                                                       || $Character
        '"funcky:types".range ("funcky:types".type ("funcky:lists".index "a"))'                                                                                                                                        || $Number
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".index ("funcky:commons".error "foo")))'                                                                                                         || $false
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
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".contains)'                                                                                                                                             || $false
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".contains)))'                                                                                         || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".contains)))'                                                                                           || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".contains))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".contains)))' || $true
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".contains))'                                                                                                                          || $Boolean
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".contains [0]))'                                                                                                                                          || $Number
        '"funcky:types".range ("funcky:types".type ("funcky:lists".contains [0]))'                                                                                                                                           || $Boolean
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".contains "a"))'                                                                                                                                          || $Character
        '"funcky:types".range ("funcky:types".type ("funcky:lists".contains "a"))'                                                                                                                                           || $Boolean
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".contains ("funcky:commons".error "foo")))'                                                                                                            || $false
        '"funcky:lists".contains [] 0'                                                                                                                                                                                       || $false
        '"funcky:lists".contains [0] 0'                                                                                                                                                                                      || $true
        '"funcky:lists".contains [0] 1'                                                                                                                                                                                      || $false
        '"funcky:lists".contains [0, 1] 0'                                                                                                                                                                                   || $true
        '"funcky:lists".contains [0, 1] 1'                                                                                                                                                                                   || $true
        '"funcky:lists".contains "" \'a\''                                                                                                                                                                                   || $false
        '"funcky:lists".contains "a" \'a\''                                                                                                                                                                                  || $true
        '"funcky:lists".contains "a" \'b\''                                                                                                                                                                                  || $false
        '"funcky:lists".contains "ab" \'a\''                                                                                                                                                                                 || $true
        '"funcky:lists".contains "ab" \'b\''                                                                                                                                                                                 || $true
        '"funcky:lists".contains [] ("funcky:commons".error "foo")'                                                                                                                                                          || $false
    }

    @Unroll('Test filter (expression: #expression)')
    def 'Test filter'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".filter)'                                                                                                                                                                   || $false
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".filter)))'                                                                                                               || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))'                                                                                         || $true
        '"funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".filter)))'                                                                                                                        || $Boolean
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))'                                                                                         || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".filter))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))' || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".filter))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))' || $true
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter [0])))'                                                                                                                                        || $Number
        '"funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter [0])))'                                                                                                                                         || $Boolean
        '"funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".filter [0])))'                                                                                                                                        || $Number
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter "a")))'                                                                                                                                        || $Character
        '"funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter "a")))'                                                                                                                                         || $Boolean
        '"funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".filter "a")))'                                                                                                                                        || $Character
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".filter ("funcky:commons".error "foo")))'                                                                                                                                  || $false
        '"funcky:lists".filter [] ("funcky:commons".lessEqual 0)'                                                                                                                                                                                || engine.converter.convert([])
        '"funcky:lists".filter [0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                               || engine.converter.convert([0.0G])
        '"funcky:lists".filter [1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                               || engine.converter.convert([1.0G])
        '"funcky:lists".filter [-1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                              || engine.converter.convert([])
        '"funcky:lists".filter [0, 0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || engine.converter.convert([0.0G, 0.0G])
        '"funcky:lists".filter [0, 1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || engine.converter.convert([0.0G, 1.0G])
        '"funcky:lists".filter [0, -1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || engine.converter.convert([0.0G])
        '"funcky:lists".filter [1, 0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || engine.converter.convert([1.0G, 0.0G])
        '"funcky:lists".filter [1, 1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || engine.converter.convert([1.0G, 1.0G])
        '"funcky:lists".filter [1, -1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || engine.converter.convert([1.0G])
        '"funcky:lists".filter [-1, 0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || engine.converter.convert([0.0G])
        '"funcky:lists".filter [-1, 1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || engine.converter.convert([1.0G])
        '"funcky:lists".filter [-1, -1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                          || engine.converter.convert([])
        '"funcky:lists".filter "" ("funcky:commons".equal \'a\')'                                                                                                                                                                                || engine.converter.convert('')
        '"funcky:lists".filter "a" ("funcky:commons".equal \'a\')'                                                                                                                                                                               || engine.converter.convert('a')
        '"funcky:lists".filter "b" ("funcky:commons".equal \'a\')'                                                                                                                                                                               || engine.converter.convert('')
        '"funcky:lists".filter "aa" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || engine.converter.convert('aa')
        '"funcky:lists".filter "ab" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || engine.converter.convert('a')
        '"funcky:lists".filter "ba" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || engine.converter.convert('a')
        '"funcky:lists".filter "bb" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || engine.converter.convert('')
        '"funcky:lists".filter [] ("funcky:commons".error "foo")'                                                                                                                                                                                || engine.converter.convert([])
    }

    @Unroll('Test map (expression: #expression)')
    def 'Test map'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                              || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".map)'                                                                                                                                                                                     || $false
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".map)))'                                                                                                                                 || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                                                                                                           || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                                                                                                            || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                                                                                                           || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".map))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                      || $true
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map)))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".map))))' || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".map))) ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                       || $false
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".map [0])))'                                                                                                                                                          || $Number
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".map [0]))))'                                                                                                                             || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".map [0]))))'                                                                                                                            || $true
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".map "a")))'                                                                                                                                                          || $Character
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".map "a"))))'                                                                                                                             || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".map "a"))))'                                                                                                                            || $true
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".map ("funcky:commons".error "foo")))'                                                                                                                                                    || $false
        '"funcky:lists".map [] ("funcky:numbers".add 1)'                                                                                                                                                                                                        || engine.converter.convert([])
        '"funcky:lists".map [0] ("funcky:numbers".add 1)'                                                                                                                                                                                                       || engine.converter.convert([1.0G])
        '"funcky:lists".map [0, 1] ("funcky:numbers".add 1)'                                                                                                                                                                                                    || engine.converter.convert([1.0G, 2.0G])
        '"funcky:lists".map "" "funcky:characters".uppercase'                                                                                                                                                                                                   || engine.converter.convert('')
        '"funcky:lists".map "a" "funcky:characters".uppercase'                                                                                                                                                                                                  || engine.converter.convert('A')
        '"funcky:lists".map "ab" "funcky:characters".uppercase'                                                                                                                                                                                                 || engine.converter.convert('AB')
        '"funcky:lists".map [] ("funcky:commons".error "foo")'                                                                                                                                                                                                  || engine.converter.convert([])
    }

    @Unroll('Test reduce (expression: #expression)')
    def 'Test reduce'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                                                                                  || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".reduce)'                                                                                                                                                                                                                                      || $false
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".reduce)))'                                                                                                                                                                                  || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                                                                                                            || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))'                                                                                                                                     || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))'                                                                                                                                      || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                                                                                                             || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                                                                                                              || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".reduce))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                    || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))' || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                        || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                         || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".reduce))) ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))'                                             || $false
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0])))'                                                                                                                                                                                                           || $Number
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0])))))'                                                                                                                                                      || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0])))))'                                                                                                                                                       || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce [0]))))'                                                                                                                                                                              || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce [0]))))'                                                                                                                                                                               || $true
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce "a")))'                                                                                                                                                                                                           || $Character
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce "a")))))'                                                                                                                                                      || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce "a")))))'                                                                                                                                                       || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce "a"))))'                                                                                                                                                                              || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce "a"))))'                                                                                                                                                                               || $true
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0] "funcky:numbers".add))'                                                                                                                                                                                                              || $Number
        '"funcky:types".range ("funcky:types".type ("funcky:lists".reduce [0] "funcky:numbers".add))'                                                                                                                                                                                                               || $Number
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
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".sort)'                                                                                                                                          || $false
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sort)))'                                                                                      || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".sort)))'                                                                                       || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sort))) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".sort)))' || $true
        '"funcky:lists".sort []'                                                                                                                                                                                      || engine.converter.convert([])
        '"funcky:lists".sort [0]'                                                                                                                                                                                     || engine.converter.convert([0.0G])
        '"funcky:lists".sort [0, 0]'                                                                                                                                                                                  || engine.converter.convert([0.0G, 0.0G])
        '"funcky:lists".sort [0, 1]'                                                                                                                                                                                  || engine.converter.convert([0.0G, 1.0G])
        '"funcky:lists".sort [1, 0]'                                                                                                                                                                                  || engine.converter.convert([0.0G, 1.0G])
        '"funcky:lists".sort [1, 1]'                                                                                                                                                                                  || engine.converter.convert([1.0G, 1.0G])
    }

    @Unroll('Test repeat (expression: #expression)')
    def 'Test repeat'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".repeat)'                                                                                                                   || $false
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:lists".repeat))'                                                                                        || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".repeat)))'                                                                || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:lists".repeat)) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".repeat)))' || $true
        '"funcky:lists".empty ("funcky:lists".repeat 0)'                                                                                                                                         || $false
        '"funcky:lists".empty ("funcky:lists".repeat \'a\')'                                                                                                                                     || $false
        '"funcky:lists".empty ("funcky:lists".repeat ("funcky:commons".error "foo"))'                                                                                                            || $false
    }

    @Unroll('Test take (expression: #expression)')
    def 'Test take'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                           || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".take)'                                                                                                                                                                 || $false
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".take)))'                                                                                                             || $true
        '"funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".take))'                                                                                                                                             || $Number
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".take))))'                                                                                       || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".take))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".take))))' || $true
        '"funcky:types".type ("funcky:lists".take [0])'                                                                                                                                                                                      || new FunckyFunctionType(engine, $Number, new FunckyListType(engine, $Number))
        '"funcky:types".type ("funcky:lists".take "a")'                                                                                                                                                                                      || new FunckyFunctionType(engine, $Number, new FunckyListType(engine, $Character))
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".take ("funcky:commons".error "foo")))'                                                                                                                                || $false
        '"funcky:lists".take [] 0'                                                                                                                                                                                                           || engine.converter.convert([])
        '"funcky:lists".take [] 1'                                                                                                                                                                                                           || engine.converter.convert([])
        '"funcky:lists".take [0] 0'                                                                                                                                                                                                          || engine.converter.convert([])
        '"funcky:lists".take [0] 1'                                                                                                                                                                                                          || engine.converter.convert([0.0G])
        '"funcky:lists".take [0] 2'                                                                                                                                                                                                          || engine.converter.convert([0.0G])
        '"funcky:lists".take [0, 1] 0'                                                                                                                                                                                                       || engine.converter.convert([])
        '"funcky:lists".take [0, 1] 1'                                                                                                                                                                                                       || engine.converter.convert([0.0G])
        '"funcky:lists".take [0, 1] 2'                                                                                                                                                                                                       || engine.converter.convert([0.0G, 1.0G])
        '"funcky:lists".take [0, 1] 3'                                                                                                                                                                                                       || engine.converter.convert([0.0, 1.0G])
        '"funcky:lists".take "" 0'                                                                                                                                                                                                           || engine.converter.convert('')
        '"funcky:lists".take "" 1'                                                                                                                                                                                                           || engine.converter.convert('')
        '"funcky:lists".take "a" 0'                                                                                                                                                                                                          || engine.converter.convert('')
        '"funcky:lists".take "a" 1'                                                                                                                                                                                                          || engine.converter.convert('a')
        '"funcky:lists".take "a" 2'                                                                                                                                                                                                          || engine.converter.convert('a')
        '"funcky:lists".take "ab" 0'                                                                                                                                                                                                         || engine.converter.convert('')
        '"funcky:lists".take "ab" 1'                                                                                                                                                                                                         || engine.converter.convert('a')
        '"funcky:lists".take "ab" 2'                                                                                                                                                                                                         || engine.converter.convert('ab')
        '"funcky:lists".take "ab" 3'                                                                                                                                                                                                         || engine.converter.convert('ab')
        '"funcky:lists".take ("funcky:commons".error "foo") 0'                                                                                                                                                                               || engine.converter.convert([])
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
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".drop)'                                                                                                                                                                 || $false
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".drop)))'                                                                                                             || $true
        '"funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".drop))'                                                                                                                                             || $Number
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".drop))))'                                                                                       || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".drop))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".drop))))' || $true
        '"funcky:types".type ("funcky:lists".drop [0])'                                                                                                                                                                                      || new FunckyFunctionType(engine, $Number, new FunckyListType(engine, $Number))
        '"funcky:types".type ("funcky:lists".drop "a")'                                                                                                                                                                                      || new FunckyFunctionType(engine, $Number, new FunckyListType(engine, $Character))
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".drop ("funcky:commons".error "foo")))'                                                                                                                                || $false
        '"funcky:lists".drop [] 0'                                                                                                                                                                                                           || engine.converter.convert([])
        '"funcky:lists".drop [] 1'                                                                                                                                                                                                           || engine.converter.convert([])
        '"funcky:lists".drop [0] 0'                                                                                                                                                                                                          || engine.converter.convert([0.0G])
        '"funcky:lists".drop [0] 1'                                                                                                                                                                                                          || engine.converter.convert([])
        '"funcky:lists".drop [0] 2'                                                                                                                                                                                                          || engine.converter.convert([])
        '"funcky:lists".drop [0, 1] 0'                                                                                                                                                                                                       || engine.converter.convert([0.0G, 1.0G])
        '"funcky:lists".drop [0, 1] 1'                                                                                                                                                                                                       || engine.converter.convert([1.0G])
        '"funcky:lists".drop [0, 1] 2'                                                                                                                                                                                                       || engine.converter.convert([])
        '"funcky:lists".drop [0, 1] 3'                                                                                                                                                                                                       || engine.converter.convert([])
        '"funcky:lists".drop "" 0'                                                                                                                                                                                                           || engine.converter.convert('')
        '"funcky:lists".drop "" 1'                                                                                                                                                                                                           || engine.converter.convert('')
        '"funcky:lists".drop "a" 0'                                                                                                                                                                                                          || engine.converter.convert('a')
        '"funcky:lists".drop "a" 1'                                                                                                                                                                                                          || engine.converter.convert('')
        '"funcky:lists".drop "a" 2'                                                                                                                                                                                                          || engine.converter.convert('')
        '"funcky:lists".drop "ab" 0'                                                                                                                                                                                                         || engine.converter.convert('ab')
        '"funcky:lists".drop "ab" 1'                                                                                                                                                                                                         || engine.converter.convert('b')
        '"funcky:lists".drop "ab" 2'                                                                                                                                                                                                         || engine.converter.convert('')
        '"funcky:lists".drop "ab" 3'                                                                                                                                                                                                         || engine.converter.convert('')
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
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".sublist)'                                                                                                                                                                                           || $false
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sublist)))'                                                                                                                                       || $true
        '"funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".sublist))'                                                                                                                                                                       || $Number
        '"funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".sublist)))'                                                                                                                                                || $Number
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".sublist)))))'                                                                                          || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sublist))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".sublist)))))' || $true
        '"funcky:types".type ("funcky:lists".sublist [0])'                                                                                                                                                                                                                || new FunckyFunctionType(engine, $Number, $Number, new FunckyListType(engine, $Number))
        '"funcky:types".type ("funcky:lists".sublist "a")'                                                                                                                                                                                                                || new FunckyFunctionType(engine, $Number, $Number, $String)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".sublist ("funcky:commons".error "foo")))'                                                                                                                                                          || $false
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".sublist ("funcky:commons".error "foo") ("funcky:commons".error "foo")))'                                                                                                                           || $false
        '"funcky:lists".sublist [] 0 0'                                                                                                                                                                                                                                   || engine.converter.convert([])
        '"funcky:lists".sublist [0] 0 0'                                                                                                                                                                                                                                  || engine.converter.convert([])
        '"funcky:lists".sublist [0] 0 1'                                                                                                                                                                                                                                  || engine.converter.convert([0.0G])
        '"funcky:lists".sublist [0] 1 1'                                                                                                                                                                                                                                  || engine.converter.convert([])
        '"funcky:lists".sublist [0, 1] 0 0'                                                                                                                                                                                                                               || engine.converter.convert([])
        '"funcky:lists".sublist [0, 1] 0 1'                                                                                                                                                                                                                               || engine.converter.convert([0.0G])
        '"funcky:lists".sublist [0, 1] 0 2'                                                                                                                                                                                                                               || engine.converter.convert([0.0G, 1.0G])
        '"funcky:lists".sublist [0, 1] 1 1'                                                                                                                                                                                                                               || engine.converter.convert([])
        '"funcky:lists".sublist [0, 1] 1 2'                                                                                                                                                                                                                               || engine.converter.convert([1.0G])
        '"funcky:lists".sublist [0, 1] 2 2'                                                                                                                                                                                                                               || engine.converter.convert([])
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
