package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.FunckyJavaConverter
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException
import io.github.thanospapapetrou.funcky.runtime.prelude.Lists
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType
import io.github.thanospapapetrou.funcky.runtime.FunckyListType
import io.github.thanospapapetrou.funcky.runtime.FunckySimpleType
import spock.lang.Unroll

class ListsSpec extends BaseSpec {
    @Unroll('Test head (expression: #expression)')
    def 'Test head'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                           || result
        '"funcky:lists".head'                                                                                                                                                                || Lists.HEAD
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".head)))'                                                             || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".type "funcky:lists".head))'                                                                                       || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".head))) ("funcky:types".range ("funcky:types".type "funcky:lists".head))' || FunckyBoolean.TRUE
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
        '"funcky:lists".tail'                                                                                                                                                                                         || Lists.TAIL
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".tail)))'                                                                                      || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".tail)))'                                                                                       || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".tail))) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".tail)))' || FunckyBoolean.TRUE
        '"funcky:lists".tail [0]'                                                                                                                                                                                     || FunckyJavaConverter.convert(List.of())
        '"funcky:lists".tail [0, 1]'                                                                                                                                                                                  || FunckyJavaConverter.convert(List.of(1.0G))
        '"funcky:lists".tail "a"'                                                                                                                                                                                     || FunckyJavaConverter.convert('')
        '"funcky:lists".tail "ab"'                                                                                                                                                                                    || FunckyJavaConverter.convert('b')
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
        '"funcky:lists".prepend'                                                                                                                                                                                                                   || Lists.PREPEND
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".prepend)))'                                                                                                                || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".prepend)))'                                                                                                                  || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".prepend))))'                                                                                          || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".prepend))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".prepend)))'                         || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".prepend))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".prepend))))' || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type ("funcky:lists".prepend [])))'                                                                                                                                    || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".prepend []))))'                                                                                                            || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type ("funcky:lists".prepend []))) ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".prepend []))))'                                       || FunckyBoolean.FALSE
        '"funcky:types".type ("funcky:lists".prepend [0])'                                                                                                                                                                                         || new FunckyFunctionType(FunckySimpleType.NUMBER, new FunckyListType(FunckySimpleType.NUMBER))
        '"funcky:types".type ("funcky:lists".prepend "")'                                                                                                                                                                                          || new FunckyFunctionType(FunckySimpleType.CHARACTER, FunckyListType.STRING)
        '"funcky:commons".string ("funcky:lists".prepend ("funcky:commons".error "foo"))'                                                                                                                                                          || FunckyJavaConverter.convert('"funcky:lists".prepend ("funcky:commons".error "foo")')
        '"funcky:lists".prepend [] 0'                                                                                                                                                                                                              || FunckyJavaConverter.convert(List.of(0.0G))
        '"funcky:lists".prepend [1] 0'                                                                                                                                                                                                             || FunckyJavaConverter.convert(List.of(0.0G, 1.0G))
        '"funcky:lists".prepend "" \'a\''                                                                                                                                                                                                          || FunckyJavaConverter.convert('a')
        '"funcky:lists".prepend "b" \'a\''                                                                                                                                                                                                         || FunckyJavaConverter.convert('ab')
    }

    @Unroll('Test append (expression: #expression)')
    def 'Test append'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".append)'                                                                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".append)))'                                                                                                               || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".append)))'                                                                                                                 || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".append))))'                                                                                         || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".append))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".append)))'                         || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".append))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".append))))' || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type ("funcky:lists".append [])))'                                                                                                                                   || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".append []))))'                                                                                                           || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type ("funcky:lists".append []))) ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".append []))))'                                       || FunckyBoolean.FALSE
        '"funcky:types".type ("funcky:lists".append [0])'                                                                                                                                                                                        || new FunckyFunctionType(FunckySimpleType.NUMBER, new FunckyListType(FunckySimpleType.NUMBER))
        '"funcky:types".type ("funcky:lists".append "")'                                                                                                                                                                                         || new FunckyFunctionType(FunckySimpleType.CHARACTER, FunckyListType.STRING)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".append ("funcky:commons".error "foo")))'                                                                                                                                  || FunckyBoolean.FALSE
        '"funcky:lists".append [] 0'                                                                                                                                                                                                             || FunckyJavaConverter.convert(List.of(0.0G))
        '"funcky:lists".append [1] 0'                                                                                                                                                                                                            || FunckyJavaConverter.convert(List.of(1.0G, 0.0G))
        '"funcky:lists".append "" \'a\''                                                                                                                                                                                                         || FunckyJavaConverter.convert('a')
        '"funcky:lists".append "b" \'a\''                                                                                                                                                                                                        || FunckyJavaConverter.convert('ba')
    }

    @Unroll('Test concat (expression: #expression)')
    def 'Test concat'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".concat)'                                                                                                                                                                    || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".concat)))'                                                                                                                || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))'                                                                                         || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))'                                                                                          || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".concat))) ("funcky:types".element ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))' || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".concat))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".concat))))'  || FunckyBoolean.TRUE
        '"funcky:types".type ("funcky:lists".concat [0])'                                                                                                                                                                                         || new FunckyFunctionType(new FunckyListType(FunckySimpleType.NUMBER), new FunckyListType(FunckySimpleType.NUMBER))
        '"funcky:types".type ("funcky:lists".concat "")'                                                                                                                                                                                          || new FunckyFunctionType(FunckyListType.STRING, FunckyListType.STRING)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".concat ("funcky:commons".error "foo")))'                                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:lists".concat [] []'                                                                                                                                                                                                             || FunckyJavaConverter.convert(List.of())
        '"funcky:lists".concat [] [0]'                                                                                                                                                                                                            || FunckyJavaConverter.convert(List.of(0.0G))
        '"funcky:lists".concat [0] []'                                                                                                                                                                                                            || FunckyJavaConverter.convert(List.of(0.0G))
        '"funcky:lists".concat [0] [1]'                                                                                                                                                                                                           || FunckyJavaConverter.convert(List.of(0.0G, 1.0G))
        '"funcky:lists".concat "" ""'                                                                                                                                                                                                             || FunckyJavaConverter.convert('')
        '"funcky:lists".concat "" "a"'                                                                                                                                                                                                            || FunckyJavaConverter.convert('a')
        '"funcky:lists".concat "a" ""'                                                                                                                                                                                                            || FunckyJavaConverter.convert('a')
        '"funcky:lists".concat "a" "b"'                                                                                                                                                                                                           || FunckyJavaConverter.convert('ab')
    }

    @Unroll('Test singleton (expression: #expression)')
    def 'Test singleton'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                     || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".singleton)'                                                                                                                      || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:lists".singleton))'                                                                                           || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".singleton)))'                                                                   || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:lists".singleton)) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".singleton)))' || FunckyBoolean.TRUE
        '"funcky:lists".singleton 0'                                                                                                                                                                   || FunckyJavaConverter.convert(List.of(0.0G))
        '"funcky:lists".singleton \'a\''                                                                                                                                                               || FunckyJavaConverter.convert('a')
    }

    @Unroll('Test empty (expression: #expression)')
    def 'Test empty'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                || result
        '"funcky:commons".string "funcky:lists".empty'                                                                            || FunckyJavaConverter.convert('"funcky:commons".equal []')
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".empty)))' || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".type "funcky:lists".empty)'                                                         || FunckySimpleType.BOOLEAN
        '"funcky:lists".empty []'                                                                                                 || FunckyBoolean.TRUE
        '"funcky:lists".empty [0]'                                                                                                || FunckyBoolean.FALSE
        '"funcky:lists".empty ""'                                                                                                 || FunckyBoolean.TRUE
        '"funcky:lists".empty "a"'                                                                                                || FunckyBoolean.FALSE
    }

    @Unroll('Test size (expression: #expression)')
    def 'Test size'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".size)'                                                     || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".size)))' || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".type "funcky:lists".size)'                                                         || FunckySimpleType.NUMBER
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
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".index)'                                                                                                                                          || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".index)))'                                                                                      || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".index)))'                                                                                        || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".index))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".index)))' || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".index))'                                                                                                                       || FunckySimpleType.NUMBER
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".index [0]))'                                                                                                                                       || FunckySimpleType.NUMBER
        '"funcky:types".range ("funcky:types".type ("funcky:lists".index [0]))'                                                                                                                                        || FunckySimpleType.NUMBER
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".index "a"))'                                                                                                                                       || FunckySimpleType.CHARACTER
        '"funcky:types".range ("funcky:types".type ("funcky:lists".index "a"))'                                                                                                                                        || FunckySimpleType.NUMBER
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".index ("funcky:commons".error "foo")))'                                                                                                         || FunckyBoolean.FALSE
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
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".contains)'                                                                                                                                             || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".contains)))'                                                                                         || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".contains)))'                                                                                           || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".contains))) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".contains)))' || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".contains))'                                                                                                                          || FunckySimpleType.BOOLEAN
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".contains [0]))'                                                                                                                                          || FunckySimpleType.NUMBER
        '"funcky:types".range ("funcky:types".type ("funcky:lists".contains [0]))'                                                                                                                                           || FunckySimpleType.BOOLEAN
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".contains "a"))'                                                                                                                                          || FunckySimpleType.CHARACTER
        '"funcky:types".range ("funcky:types".type ("funcky:lists".contains "a"))'                                                                                                                                           || FunckySimpleType.BOOLEAN
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".contains ("funcky:commons".error "foo")))'                                                                                                            || FunckyBoolean.FALSE
        '"funcky:lists".contains [] 0'                                                                                                                                                                                       || FunckyBoolean.FALSE
        '"funcky:lists".contains [0] 0'                                                                                                                                                                                      || FunckyBoolean.TRUE
        '"funcky:lists".contains [0] 1'                                                                                                                                                                                      || FunckyBoolean.FALSE
        '"funcky:lists".contains [0, 1] 0'                                                                                                                                                                                   || FunckyBoolean.TRUE
        '"funcky:lists".contains [0, 1] 1'                                                                                                                                                                                   || FunckyBoolean.TRUE
        '"funcky:lists".contains "" \'a\''                                                                                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:lists".contains "a" \'a\''                                                                                                                                                                                  || FunckyBoolean.TRUE
        '"funcky:lists".contains "a" \'b\''                                                                                                                                                                                  || FunckyBoolean.FALSE
        '"funcky:lists".contains "ab" \'a\''                                                                                                                                                                                 || FunckyBoolean.TRUE
        '"funcky:lists".contains "ab" \'b\''                                                                                                                                                                                 || FunckyBoolean.TRUE
        '"funcky:lists".contains [] ("funcky:commons".error "foo")'                                                                                                                                                          || FunckyBoolean.FALSE
    }

    @Unroll('Test filter (expression: #expression)')
    def 'Test filter'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".filter)'                                                                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".filter)))'                                                                                                               || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))'                                                                                         || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".filter)))'                                                                                                                        || FunckySimpleType.BOOLEAN
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))'                                                                                         || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".filter))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))' || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".filter))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".filter))))' || FunckyBoolean.TRUE
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter [0])))'                                                                                                                                        || FunckySimpleType.NUMBER
        '"funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter [0])))'                                                                                                                                         || FunckySimpleType.BOOLEAN
        '"funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".filter [0])))'                                                                                                                                        || FunckySimpleType.NUMBER
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter "a")))'                                                                                                                                        || FunckySimpleType.CHARACTER
        '"funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".filter "a")))'                                                                                                                                         || FunckySimpleType.BOOLEAN
        '"funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".filter "a")))'                                                                                                                                        || FunckySimpleType.CHARACTER
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".filter ("funcky:commons".error "foo")))'                                                                                                                                  || FunckyBoolean.FALSE
        '"funcky:lists".filter [] ("funcky:commons".lessEqual 0)'                                                                                                                                                                                || FunckyJavaConverter.convert([])
        '"funcky:lists".filter [0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                               || FunckyJavaConverter.convert([0.0G])
        '"funcky:lists".filter [1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                               || FunckyJavaConverter.convert([1.0G])
        '"funcky:lists".filter [-1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                              || FunckyJavaConverter.convert([])
        '"funcky:lists".filter [0, 0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || FunckyJavaConverter.convert([0.0G, 0.0G])
        '"funcky:lists".filter [0, 1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || FunckyJavaConverter.convert([0.0G, 1.0G])
        '"funcky:lists".filter [0, -1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || FunckyJavaConverter.convert([0.0G])
        '"funcky:lists".filter [1, 0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || FunckyJavaConverter.convert([1.0G, 0.0G])
        '"funcky:lists".filter [1, 1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                            || FunckyJavaConverter.convert([1.0G, 1.0G])
        '"funcky:lists".filter [1, -1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || FunckyJavaConverter.convert([1.0G])
        '"funcky:lists".filter [-1, 0] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || FunckyJavaConverter.convert([0.0G])
        '"funcky:lists".filter [-1, 1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                           || FunckyJavaConverter.convert([1.0G])
        '"funcky:lists".filter [-1, -1] ("funcky:commons".lessEqual 0)'                                                                                                                                                                          || FunckyJavaConverter.convert([])
        '"funcky:lists".filter "" ("funcky:commons".equal \'a\')'                                                                                                                                                                                || FunckyJavaConverter.convert('')
        '"funcky:lists".filter "a" ("funcky:commons".equal \'a\')'                                                                                                                                                                               || FunckyJavaConverter.convert('a')
        '"funcky:lists".filter "b" ("funcky:commons".equal \'a\')'                                                                                                                                                                               || FunckyJavaConverter.convert('')
        '"funcky:lists".filter "aa" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || FunckyJavaConverter.convert('aa')
        '"funcky:lists".filter "ab" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || FunckyJavaConverter.convert('a')
        '"funcky:lists".filter "ba" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || FunckyJavaConverter.convert('a')
        '"funcky:lists".filter "bb" ("funcky:commons".equal \'a\')'                                                                                                                                                                              || FunckyJavaConverter.convert('')
        '"funcky:lists".filter [] ("funcky:commons".error "foo")'                                                                                                                                                                                || FunckyJavaConverter.convert([])
    }

    @Unroll('Test map (expression: #expression)')
    def 'Test map'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                              || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".map)'                                                                                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".map)))'                                                                                                                                 || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                                                                                                           || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                                                                                                            || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                                                                                                           || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".map))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                      || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map)))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".map))))' || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".map))) ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".map))))'                       || FunckyBoolean.FALSE
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".map [0])))'                                                                                                                                                          || FunckySimpleType.NUMBER
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".map [0]))))'                                                                                                                             || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".map [0]))))'                                                                                                                            || FunckyBoolean.TRUE
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".map "a")))'                                                                                                                                                          || FunckySimpleType.CHARACTER
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".map "a"))))'                                                                                                                             || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type ("funcky:lists".map "a"))))'                                                                                                                            || FunckyBoolean.TRUE
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".map ("funcky:commons".error "foo")))'                                                                                                                                                    || FunckyBoolean.FALSE
        '"funcky:lists".map [] ("funcky:numbers".add 1)'                                                                                                                                                                                                        || FunckyJavaConverter.convert([])
        '"funcky:lists".map [0] ("funcky:numbers".add 1)'                                                                                                                                                                                                       || FunckyJavaConverter.convert([1.0G])
        '"funcky:lists".map [0, 1] ("funcky:numbers".add 1)'                                                                                                                                                                                                    || FunckyJavaConverter.convert([1.0G, 2.0G])
        '"funcky:lists".map "" "funcky:characters".uppercase'                                                                                                                                                                                                   || FunckyJavaConverter.convert('')
        '"funcky:lists".map "a" "funcky:characters".uppercase'                                                                                                                                                                                                  || FunckyJavaConverter.convert('A')
        '"funcky:lists".map "ab" "funcky:characters".uppercase'                                                                                                                                                                                                 || FunckyJavaConverter.convert('AB')
        '"funcky:lists".map [] ("funcky:commons".error "foo")'                                                                                                                                                                                                  || FunckyJavaConverter.convert([])
    }

    @Unroll('Test reduce (expression: #expression)')
    def 'Test reduce'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                                                                                                  || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".reduce)'                                                                                                                                                                                                                                      || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".reduce)))'                                                                                                                                                                                  || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                                                                                                            || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))'                                                                                                                                     || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))'                                                                                                                                      || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                                                                                                             || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                                                                                                              || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".reduce))) ("funcky:types".domain ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                                                                    || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))) ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))' || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                        || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".reduce))))'                         || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".reduce))) ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".reduce)))))'                                             || FunckyBoolean.FALSE
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0])))'                                                                                                                                                                                                           || FunckySimpleType.NUMBER
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0])))))'                                                                                                                                                      || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0])))))'                                                                                                                                                       || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce [0]))))'                                                                                                                                                                              || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce [0]))))'                                                                                                                                                                               || FunckyBoolean.TRUE
        '"funcky:types".domain ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce "a")))'                                                                                                                                                                                                           || FunckySimpleType.CHARACTER
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce "a")))))'                                                                                                                                                      || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".domain ("funcky:types".type ("funcky:lists".reduce "a")))))'                                                                                                                                                       || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce "a"))))'                                                                                                                                                                              || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".type ("funcky:lists".reduce "a"))))'                                                                                                                                                                               || FunckyBoolean.TRUE
        '"funcky:types".domain ("funcky:types".type ("funcky:lists".reduce [0] "funcky:numbers".add))'                                                                                                                                                                                                              || FunckySimpleType.NUMBER
        '"funcky:types".range ("funcky:types".type ("funcky:lists".reduce [0] "funcky:numbers".add))'                                                                                                                                                                                                               || FunckySimpleType.NUMBER
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
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".sort)'                                                                                                                                          || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sort)))'                                                                                      || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".sort)))'                                                                                       || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sort))) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".sort)))' || FunckyBoolean.TRUE
        '"funcky:lists".sort []'                                                                                                                                                                                      || FunckyJavaConverter.convert([])
        '"funcky:lists".sort [0]'                                                                                                                                                                                     || FunckyJavaConverter.convert([0.0G])
        '"funcky:lists".sort [0, 0]'                                                                                                                                                                                  || FunckyJavaConverter.convert([0.0G, 0.0G])
        '"funcky:lists".sort [0, 1]'                                                                                                                                                                                  || FunckyJavaConverter.convert([0.0G, 1.0G])
        '"funcky:lists".sort [1, 0]'                                                                                                                                                                                  || FunckyJavaConverter.convert([0.0G, 1.0G])
        '"funcky:lists".sort [1, 1]'                                                                                                                                                                                  || FunckyJavaConverter.convert([1.0G, 1.0G])
    }

    @Unroll('Test repeat (expression: #expression)')
    def 'Test repeat'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                               || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".repeat)'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:lists".repeat))'                                                                                        || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".repeat)))'                                                                || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:lists".repeat)) ("funcky:types".element ("funcky:types".range ("funcky:types".type "funcky:lists".repeat)))' || FunckyBoolean.TRUE
        '"funcky:lists".empty ("funcky:lists".repeat 0)'                                                                                                                                         || FunckyBoolean.FALSE
        '"funcky:lists".empty ("funcky:lists".repeat \'a\')'                                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:lists".empty ("funcky:lists".repeat ("funcky:commons".error "foo"))'                                                                                                            || FunckyBoolean.FALSE
    }

    @Unroll('Test take (expression: #expression)')
    def 'Test take'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                           || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".take)'                                                                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".take)))'                                                                                                             || FunckyBoolean.TRUE
        '"funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".take))'                                                                                                                                             || FunckySimpleType.NUMBER
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".take))))'                                                                                       || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".take))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".take))))' || FunckyBoolean.TRUE
        '"funcky:types".type ("funcky:lists".take [0])'                                                                                                                                                                                      || new FunckyFunctionType(FunckySimpleType.NUMBER, new FunckyListType(FunckySimpleType.NUMBER))
        '"funcky:types".type ("funcky:lists".take "a")'                                                                                                                                                                                      || new FunckyFunctionType(FunckySimpleType.NUMBER, new FunckyListType(FunckySimpleType.CHARACTER))
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".take ("funcky:commons".error "foo")))'                                                                                                                                || FunckyBoolean.FALSE
        '"funcky:lists".take [] 0'                                                                                                                                                                                                           || FunckyJavaConverter.convert([])
        '"funcky:lists".take [] 1'                                                                                                                                                                                                           || FunckyJavaConverter.convert([])
        '"funcky:lists".take [0] 0'                                                                                                                                                                                                          || FunckyJavaConverter.convert([])
        '"funcky:lists".take [0] 1'                                                                                                                                                                                                          || FunckyJavaConverter.convert([0.0G])
        '"funcky:lists".take [0] 2'                                                                                                                                                                                                          || FunckyJavaConverter.convert([0.0G])
        '"funcky:lists".take [0, 1] 0'                                                                                                                                                                                                       || FunckyJavaConverter.convert([])
        '"funcky:lists".take [0, 1] 1'                                                                                                                                                                                                       || FunckyJavaConverter.convert([0.0G])
        '"funcky:lists".take [0, 1] 2'                                                                                                                                                                                                       || FunckyJavaConverter.convert([0.0G, 1.0G])
        '"funcky:lists".take [0, 1] 3'                                                                                                                                                                                                       || FunckyJavaConverter.convert([0.0, 1.0G])
        '"funcky:lists".take "" 0'                                                                                                                                                                                                           || FunckyJavaConverter.convert('')
        '"funcky:lists".take "" 1'                                                                                                                                                                                                           || FunckyJavaConverter.convert('')
        '"funcky:lists".take "a" 0'                                                                                                                                                                                                          || FunckyJavaConverter.convert('')
        '"funcky:lists".take "a" 1'                                                                                                                                                                                                          || FunckyJavaConverter.convert('a')
        '"funcky:lists".take "a" 2'                                                                                                                                                                                                          || FunckyJavaConverter.convert('a')
        '"funcky:lists".take "ab" 0'                                                                                                                                                                                                         || FunckyJavaConverter.convert('')
        '"funcky:lists".take "ab" 1'                                                                                                                                                                                                         || FunckyJavaConverter.convert('a')
        '"funcky:lists".take "ab" 2'                                                                                                                                                                                                         || FunckyJavaConverter.convert('ab')
        '"funcky:lists".take "ab" 3'                                                                                                                                                                                                         || FunckyJavaConverter.convert('ab')
        '"funcky:lists".take ("funcky:commons".error "foo") 0'                                                                                                                                                                               || FunckyJavaConverter.convert([])
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
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".drop)'                                                                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".drop)))'                                                                                                             || FunckyBoolean.TRUE
        '"funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".drop))'                                                                                                                                             || FunckySimpleType.NUMBER
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".drop))))'                                                                                       || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".drop))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".drop))))' || FunckyBoolean.TRUE
        '"funcky:types".type ("funcky:lists".drop [0])'                                                                                                                                                                                      || new FunckyFunctionType(FunckySimpleType.NUMBER, new FunckyListType(FunckySimpleType.NUMBER))
        '"funcky:types".type ("funcky:lists".drop "a")'                                                                                                                                                                                      || new FunckyFunctionType(FunckySimpleType.NUMBER, new FunckyListType(FunckySimpleType.CHARACTER))
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".drop ("funcky:commons".error "foo")))'                                                                                                                                || FunckyBoolean.FALSE
        '"funcky:lists".drop [] 0'                                                                                                                                                                                                           || FunckyJavaConverter.convert([])
        '"funcky:lists".drop [] 1'                                                                                                                                                                                                           || FunckyJavaConverter.convert([])
        '"funcky:lists".drop [0] 0'                                                                                                                                                                                                          || FunckyJavaConverter.convert([0.0G])
        '"funcky:lists".drop [0] 1'                                                                                                                                                                                                          || FunckyJavaConverter.convert([])
        '"funcky:lists".drop [0] 2'                                                                                                                                                                                                          || FunckyJavaConverter.convert([])
        '"funcky:lists".drop [0, 1] 0'                                                                                                                                                                                                       || FunckyJavaConverter.convert([0.0G, 1.0G])
        '"funcky:lists".drop [0, 1] 1'                                                                                                                                                                                                       || FunckyJavaConverter.convert([1.0G])
        '"funcky:lists".drop [0, 1] 2'                                                                                                                                                                                                       || FunckyJavaConverter.convert([])
        '"funcky:lists".drop [0, 1] 3'                                                                                                                                                                                                       || FunckyJavaConverter.convert([])
        '"funcky:lists".drop "" 0'                                                                                                                                                                                                           || FunckyJavaConverter.convert('')
        '"funcky:lists".drop "" 1'                                                                                                                                                                                                           || FunckyJavaConverter.convert('')
        '"funcky:lists".drop "a" 0'                                                                                                                                                                                                          || FunckyJavaConverter.convert('a')
        '"funcky:lists".drop "a" 1'                                                                                                                                                                                                          || FunckyJavaConverter.convert('')
        '"funcky:lists".drop "a" 2'                                                                                                                                                                                                          || FunckyJavaConverter.convert('')
        '"funcky:lists".drop "ab" 0'                                                                                                                                                                                                         || FunckyJavaConverter.convert('ab')
        '"funcky:lists".drop "ab" 1'                                                                                                                                                                                                         || FunckyJavaConverter.convert('b')
        '"funcky:lists".drop "ab" 2'                                                                                                                                                                                                         || FunckyJavaConverter.convert('')
        '"funcky:lists".drop "ab" 3'                                                                                                                                                                                                         || FunckyJavaConverter.convert('')
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
        '"funcky:lists".empty ("funcky:commons".string "funcky:lists".sublist)'                                                                                                                                                                                           || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sublist)))'                                                                                                                                       || FunckyBoolean.TRUE
        '"funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:lists".sublist))'                                                                                                                                                                       || FunckySimpleType.NUMBER
        '"funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".sublist)))'                                                                                                                                                || FunckySimpleType.NUMBER
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".sublist)))))'                                                                                          || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".domain ("funcky:types".type "funcky:lists".sublist))) ("funcky:types".element ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:lists".sublist)))))' || FunckyBoolean.TRUE
        '"funcky:types".type ("funcky:lists".sublist [0])'                                                                                                                                                                                                                || new FunckyFunctionType(FunckySimpleType.NUMBER, FunckySimpleType.NUMBER, new FunckyListType(FunckySimpleType.NUMBER))
        '"funcky:types".type ("funcky:lists".sublist "a")'                                                                                                                                                                                                                || new FunckyFunctionType(FunckySimpleType.NUMBER, FunckySimpleType.NUMBER, FunckyListType.STRING)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".sublist ("funcky:commons".error "foo")))'                                                                                                                                                          || FunckyBoolean.FALSE
        '"funcky:lists".empty ("funcky:commons".string ("funcky:lists".sublist ("funcky:commons".error "foo") ("funcky:commons".error "foo")))'                                                                                                                           || FunckyBoolean.FALSE
        '"funcky:lists".sublist [] 0 0'                                                                                                                                                                                                                                   || FunckyJavaConverter.convert([])
        '"funcky:lists".sublist [0] 0 0'                                                                                                                                                                                                                                  || FunckyJavaConverter.convert([])
        '"funcky:lists".sublist [0] 0 1'                                                                                                                                                                                                                                  || FunckyJavaConverter.convert([0.0G])
        '"funcky:lists".sublist [0] 1 1'                                                                                                                                                                                                                                  || FunckyJavaConverter.convert([])
        '"funcky:lists".sublist [0, 1] 0 0'                                                                                                                                                                                                                               || FunckyJavaConverter.convert([])
        '"funcky:lists".sublist [0, 1] 0 1'                                                                                                                                                                                                                               || FunckyJavaConverter.convert([0.0G])
        '"funcky:lists".sublist [0, 1] 0 2'                                                                                                                                                                                                                               || FunckyJavaConverter.convert([0.0G, 1.0G])
        '"funcky:lists".sublist [0, 1] 1 1'                                                                                                                                                                                                                               || FunckyJavaConverter.convert([])
        '"funcky:lists".sublist [0, 1] 1 2'                                                                                                                                                                                                                               || FunckyJavaConverter.convert([1.0G])
        '"funcky:lists".sublist [0, 1] 2 2'                                                                                                                                                                                                                               || FunckyJavaConverter.convert([])
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
