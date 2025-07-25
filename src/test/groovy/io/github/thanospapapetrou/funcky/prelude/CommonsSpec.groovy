package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.FunckyJavaConverter
import io.github.thanospapapetrou.funcky.runtime.FunckyBoolean
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException
import io.github.thanospapapetrou.funcky.runtime.prelude.Commons
import io.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType
import io.github.thanospapapetrou.funcky.runtime.types.FunckyListType
import io.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType
import spock.lang.Unroll

class CommonsSpec extends BaseSpec {
    @Unroll('Test equal (expression: #expression)')
    def 'Test equal'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                || result
        '"funcky:commons".equal'                                                                                                                                                                  || Commons.EQUAL
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".equal))'                                                                                        || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".equal)))'                                                                 || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:commons".equal)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".equal)))' || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".equal))'                                                                                                || FunckySimpleType.BOOLEAN
        '"funcky:types".type ("funcky:commons".equal "funcky:types".Type)'                                                                                                                        || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN)
        '"funcky:types".type ("funcky:commons".equal 0)'                                                                                                                                          || new FunckyFunctionType(FunckySimpleType.NUMBER, FunckySimpleType.BOOLEAN)
        '"funcky:commons".string ("funcky:commons".equal ("funcky:commons".error "foo"))'                                                                                                         || FunckyJavaConverter.convert('"funcky:commons".equal ("funcky:commons".error "foo")')
        '"funcky:commons".equal "funcky:types".Type "funcky:types".Type'                                                                                                                          || FunckyBoolean.TRUE
        '"funcky:commons".equal "funcky:types".Type "funcky:types".Number'                                                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Type "funcky:types".Boolean'                                                                                                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Type "funcky:types".Character'                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".Function "funcky:types".Type $_)'                                                                                             || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".Function $_ "funcky:types".Number)'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".Function $_ $_)'                                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".List "funcky:types".Type)'                                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".List $_)'                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Type "funcky:types".String'                                                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".Record [])'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".Record ["funcky:types".Type])'                                                                                                || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".Record [$_])'                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Type "funcky:types".Unit'                                                                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Type $_'                                                                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number "funcky:types".Type'                                                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number "funcky:types".Number'                                                                                                                      || FunckyBoolean.TRUE
        '"funcky:commons".equal "funcky:types".Number "funcky:types".Boolean'                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number "funcky:types".Character'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".Function "funcky:types".Type $_)'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".Function $_ "funcky:types".Number)'                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".Function $_ $_)'                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".List "funcky:types".Type)'                                                                                                  || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".List $_)'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number "funcky:types".String'                                                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".Record [])'                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".Record ["funcky:types".Type])'                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".Record [$_])'                                                                                                               || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number "funcky:types".Unit'                                                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Number $_'                                                                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean "funcky:types".Type'                                                                                                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean "funcky:types".Number'                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean "funcky:types".Boolean'                                                                                                                    || FunckyBoolean.TRUE
        '"funcky:commons".equal "funcky:types".Boolean "funcky:types".Character'                                                                                                                  || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".Function "funcky:types".Type $_)'                                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".Function $_ "funcky:types".Number)'                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".Function $_ $_)'                                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".List "funcky:types".Type)'                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".List $_)'                                                                                                                  || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean "funcky:types".String'                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".Record [])'                                                                                                                || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".Record ["funcky:types".Type])'                                                                                             || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".Record [$_])'                                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean "funcky:types".Unit'                                                                                                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Boolean $_'                                                                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character "funcky:types".Type'                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character "funcky:types".Number'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character "funcky:types".Boolean'                                                                                                                  || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character "funcky:types".Character'                                                                                                                || FunckyBoolean.TRUE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".Function "funcky:types".Type $_)'                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".Function $_ "funcky:types".Number)'                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".Function $_ $_)'                                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".List "funcky:types".Type)'                                                                                               || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".List $_)'                                                                                                                || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character "funcky:types".String'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".Record [])'                                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".Record ["funcky:types".Type])'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".Record [$_])'                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character "funcky:types".Unit'                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Character $_'                                                                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Type'                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Number'                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Boolean'                                                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Character'                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                          || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function "funcky:types".Type $_)'                                             || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number)'                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function $_ $_)'                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".List "funcky:types".Type)'                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".List $_)'                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".String'                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Record [])'                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Record ["funcky:types".Type])'                                                || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Record [$_])'                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Unit'                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) $_'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Type'                                                                                             || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Number'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Boolean'                                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Character'                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                             || FunckyBoolean.FALSE
        '"funcky:types".type ("funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type $_))'                                          || FunckySimpleType.BOOLEAN
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ "funcky:types".Number)'                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ $_)'                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".List "funcky:types".Type)'                                                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".List $_)'                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) "funcky:types".String'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Record [])'                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Record ["funcky:types".Type])'                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Record [$_])'                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Unit'                                                                                             || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) $_'                                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Type'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Number'                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Boolean'                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Character'                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function "funcky:types".Type $_)'                                                              || FunckyBoolean.FALSE
        '"funcky:types".type ("funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number))'                                      || FunckySimpleType.BOOLEAN
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ $_)'                                                                               || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".List "funcky:types".Type)'                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".List $_)'                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".String'                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Record [])'                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Record ["funcky:types".Type])'                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Record [$_])'                                                                                  || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Unit'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) $_'                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) "funcky:types".Type'                                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) "funcky:types".Number'                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) "funcky:types".Boolean'                                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) "funcky:types".Character'                                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".Function "funcky:types".Type $_)'                                                                                 || FunckyBoolean.FALSE
        '"funcky:types".type ("funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".Function $_ "funcky:types".Number))'                                                         || FunckySimpleType.BOOLEAN
        '"funcky:types".type ("funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".Function $_ $_))'                                                                            || FunckySimpleType.BOOLEAN
        '"funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".List "funcky:types".Type)'                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".List $_)'                                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) "funcky:types".String'                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".Record [])'                                                                                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".Record ["funcky:types".Type])'                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".Record [$_])'                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) "funcky:types".Unit'                                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) $_'                                                                                                                               || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) "funcky:types".Type'                                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) "funcky:types".Number'                                                                                                  || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) "funcky:types".Boolean'                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) "funcky:types".Character'                                                                                               || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".Function "funcky:types".Type $_)'                                                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".Function $_ "funcky:types".Number)'                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".Function $_ $_)'                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".List "funcky:types".Type)'                                                                              || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".List $_)'                                                                                               || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) "funcky:types".String'                                                                                                  || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".Record [])'                                                                                             || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".Record ["funcky:types".Type])'                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".Record [$_])'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) "funcky:types".Unit'                                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) $_'                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List $_) "funcky:types".Type'                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List $_) "funcky:types".Number'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List $_) "funcky:types".Boolean'                                                                                                                  || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List $_) "funcky:types".Character'                                                                                                                || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".Function "funcky:types".Type $_)'                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".Function $_ "funcky:types".Number)'                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".Function $_ $_)'                                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".List "funcky:types".Type)'                                                                                               || FunckyBoolean.FALSE
        '"funcky:types".type ("funcky:commons".equal ("funcky:types".List $_) ("funcky:types".List $_))'                                                                                          || FunckySimpleType.BOOLEAN
        '"funcky:commons".equal ("funcky:types".List $_) "funcky:types".String'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".Record [])'                                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".Record ["funcky:types".Type])'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".Record [$_])'                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List $_) "funcky:types".Unit'                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".List $_) $_'                                                                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String "funcky:types".Type'                                                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String "funcky:types".Number'                                                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String "funcky:types".Boolean'                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String "funcky:types".Character'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".Function "funcky:types".Type $_)'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".Function $_ "funcky:types".Number)'                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".Function $_ $_)'                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".List "funcky:types".Type)'                                                                                                  || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".List $_)'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String "funcky:types".String'                                                                                                                      || FunckyBoolean.TRUE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".Record [])'                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".Record ["funcky:types".Type])'                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".Record [$_])'                                                                                                               || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String "funcky:types".Unit'                                                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".String $_'                                                                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record []) "funcky:types".Type'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record []) "funcky:types".Number'                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record []) "funcky:types".Boolean'                                                                                                                || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record []) "funcky:types".Character'                                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".Function "funcky:types".Type $_)'                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".Function $_ "funcky:types".Number)'                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".Function $_ $_)'                                                                                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".List "funcky:types".Type)'                                                                                             || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".List $_)'                                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record []) "funcky:types".String'                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".Record [])'                                                                                                            || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".Record ["funcky:types".Type])'                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".Record [$_])'                                                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record []) "funcky:types".Unit'                                                                                                                   || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".Record []) $_'                                                                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Type'                                                                                                || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Number'                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Boolean'                                                                                             || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Character'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function "funcky:types".Type $_)'                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function $_ "funcky:types".Number)'                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function $_ $_)'                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".List "funcky:types".Type)'                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".List $_)'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) "funcky:types".String'                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record [])'                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record ["funcky:types".Type])'                                                                      || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record ["funcky:types".Number])'                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record [$_])'                                                                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Unit'                                                                                                || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) $_'                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) "funcky:types".Type'                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) "funcky:types".Number'                                                                                                               || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) "funcky:types".Boolean'                                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) "funcky:types".Character'                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".Function "funcky:types".Type $_)'                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".Function $_ "funcky:types".Number)'                                                                                  || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".Function $_ $_)'                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".List "funcky:types".Type)'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".List $_)'                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) "funcky:types".String'                                                                                                               || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".Record [])'                                                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".Record ["funcky:types".Type])'                                                                                       || FunckyBoolean.FALSE
        '"funcky:types".type ("funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".Record [$_]))'                                                                                  || FunckySimpleType.BOOLEAN
        '"funcky:commons".equal ("funcky:types".Record [$_]) "funcky:types".Unit'                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) $_'                                                                                                                                  || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Unit "funcky:types".Type'                                                                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Unit "funcky:types".Number'                                                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Unit "funcky:types".Boolean'                                                                                                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Unit "funcky:types".Character'                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".Function "funcky:types".Type $_)'                                                                                             || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".Function $_ "funcky:types".Number)'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".Function $_ $_)'                                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".List "funcky:types".Type)'                                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".List $_)'                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Unit "funcky:types".String'                                                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".Record [])'                                                                                                                   || FunckyBoolean.TRUE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".Record ["funcky:types".Type])'                                                                                                || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".Record [$_])'                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:types".Unit "funcky:types".Unit'                                                                                                                          || FunckyBoolean.TRUE
        '"funcky:commons".equal "funcky:types".Unit $_'                                                                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ "funcky:types".Type'                                                                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ "funcky:types".Number'                                                                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ "funcky:types".Boolean'                                                                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ "funcky:types".Character'                                                                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ ("funcky:types".Function "funcky:types".Type $_)'                                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ ("funcky:types".Function $_ "funcky:types".Number)'                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ ("funcky:types".Function $_ $_)'                                                                                                                               || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ ("funcky:types".List "funcky:types".Type)'                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ ("funcky:types".List $_)'                                                                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ "funcky:types".String'                                                                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ ("funcky:types".Record [])'                                                                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ ("funcky:types".Record ["funcky:types".Type])'                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ ("funcky:types".Record [$_])'                                                                                                                                  || FunckyBoolean.FALSE
        '"funcky:commons".equal $_ "funcky:types".Unit'                                                                                                                                           || FunckyBoolean.FALSE
        '"funcky:types".type ("funcky:commons".equal $_ $_)'                                                                                                                                      || FunckySimpleType.BOOLEAN
        '"funcky:commons".equal 0 0'                                                                                                                                                              || FunckyBoolean.TRUE
        '"funcky:commons".equal 0 1'                                                                                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal 1 0'                                                                                                                                                              || FunckyBoolean.FALSE
        '"funcky:commons".equal 1 1'                                                                                                                                                              || FunckyBoolean.TRUE
        '"funcky:commons".equal "funcky:booleans".false "funcky:booleans".false'                                                                                                                  || FunckyBoolean.TRUE
        '"funcky:commons".equal "funcky:booleans".false "funcky:booleans".true'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:booleans".true "funcky:booleans".false'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:booleans".true "funcky:booleans".true'                                                                                                                    || FunckyBoolean.TRUE
        '"funcky:commons".equal \'a\' \'a\''                                                                                                                                                      || FunckyBoolean.TRUE
        '"funcky:commons".equal \'a\' \'b\''                                                                                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".equal \'b\' \'a\''                                                                                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".equal \'b\' \'b\''                                                                                                                                                      || FunckyBoolean.TRUE
        '"funcky:commons".equal "funcky:numbers".add "funcky:numbers".add'                                                                                                                        || FunckyBoolean.TRUE
        '"funcky:commons".equal "funcky:numbers".add "funcky:numbers".subtract'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:numbers".subtract "funcky:numbers".add'                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal "funcky:numbers".subtract "funcky:numbers".subtract'                                                                                                              || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:numbers".add 0) ("funcky:numbers".add 0)'                                                                                                                || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:numbers".add 0) ("funcky:numbers".add 1)'                                                                                                                || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:numbers".add 1) ("funcky:numbers".add 0)'                                                                                                                || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:numbers".add 1) ("funcky:numbers".add 1)'                                                                                                                || FunckyBoolean.TRUE
        '"funcky:commons".equal [] []'                                                                                                                                                            || FunckyBoolean.TRUE
        '"funcky:commons".equal [] [0]'                                                                                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal [] [0, 1]'                                                                                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal [0] []'                                                                                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal [0] [0]'                                                                                                                                                          || FunckyBoolean.TRUE
        '"funcky:commons".equal [0] [0, 1]'                                                                                                                                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal [0, 1] []'                                                                                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".equal [0, 1] [0]'                                                                                                                                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal [0, 1] [0, 1]'                                                                                                                                                    || FunckyBoolean.TRUE
        '"funcky:commons".equal "" ""'                                                                                                                                                            || FunckyBoolean.TRUE
        '"funcky:commons".equal "" "a"'                                                                                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal "" "b"'                                                                                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal "a" ""'                                                                                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal "a" "a"'                                                                                                                                                          || FunckyBoolean.TRUE
        '"funcky:commons".equal "a" "b"'                                                                                                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal "b" ""'                                                                                                                                                           || FunckyBoolean.FALSE
        '"funcky:commons".equal "b" "a"'                                                                                                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal "b" "b"'                                                                                                                                                          || FunckyBoolean.TRUE
        '"funcky:commons".equal {} {}'                                                                                                                                                            || FunckyBoolean.TRUE
        '"funcky:commons".equal {0} {0}'                                                                                                                                                          || FunckyBoolean.TRUE
        '"funcky:commons".equal {0} {1}'                                                                                                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal {1} {0}'                                                                                                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".equal {1} {1}'                                                                                                                                                          || FunckyBoolean.TRUE
        '"funcky:commons".equal {0, \'a\'} {0, \'a\'}'                                                                                                                                            || FunckyBoolean.TRUE
        '"funcky:commons".equal {0, \'a\'} {0, \'b\'}'                                                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal {0, \'a\'} {1, \'a\'}'                                                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal {0, \'a\'} {1, \'b\'}'                                                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal {0, \'b\'} {0, \'a\'}'                                                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal {0, \'b\'} {0, \'b\'}'                                                                                                                                            || FunckyBoolean.TRUE
        '"funcky:commons".equal {0, \'b\'} {1, \'a\'}'                                                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal {0, \'b\'} {1, \'b\'}'                                                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal {1, \'a\'} {0, \'a\'}'                                                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal {1, \'a\'} {0, \'b\'}'                                                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal {1, \'a\'} {1, \'a\'}'                                                                                                                                            || FunckyBoolean.TRUE
        '"funcky:commons".equal {1, \'a\'} {1, \'b\'}'                                                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal {1, \'b\'} {0, \'a\'}'                                                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal {1, \'b\'} {0, \'b\'}'                                                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal {1, \'b\'} {1, \'a\'}'                                                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".equal {1, \'b\'} {1, \'b\'}'                                                                                                                                            || FunckyBoolean.TRUE
    }

    @Unroll('Test greaterThan (expression: #expression)')
    def 'Test greaterThan'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                            || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:commons".greaterThan)'                                                                                                                         || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".greaterThan))'                                                                                              || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".greaterThan)))'                                                                       || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:commons".greaterThan)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".greaterThan)))' || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".greaterThan))'                                                                                                      || FunckySimpleType.BOOLEAN
        '"funcky:types".type ("funcky:commons".greaterThan 0)'                                                                                                                                                || new FunckyFunctionType(FunckySimpleType.NUMBER, FunckySimpleType.BOOLEAN)
        '"funcky:types".type ("funcky:commons".greaterThan \'a\')'                                                                                                                                            || new FunckyFunctionType(FunckySimpleType.CHARACTER, FunckySimpleType.BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:commons".greaterThan ("funcky:commons".error "foo")))'                                                                                        || FunckyBoolean.FALSE
        '"funcky:commons".greaterThan 0 0'                                                                                                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".greaterThan 0 1'                                                                                                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".greaterThan 1 0'                                                                                                                                                                    || FunckyBoolean.TRUE
        '"funcky:commons".greaterThan 1 1'                                                                                                                                                                    || FunckyBoolean.FALSE
        '"funcky:commons".greaterThan \'a\' \'a\''                                                                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".greaterThan \'a\' \'b\''                                                                                                                                                            || FunckyBoolean.FALSE
        '"funcky:commons".greaterThan \'b\' \'a\''                                                                                                                                                            || FunckyBoolean.TRUE
        '"funcky:commons".greaterThan \'b\' \'b\''                                                                                                                                                            || FunckyBoolean.FALSE
    }

    @Unroll('Test lessThan (expression: #expression)')
    def 'Test lessThan'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                      || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:commons".lessThan)'                                                                                                                      || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".lessThan))'                                                                                           || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".lessThan)))'                                                                    || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:commons".lessThan)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".lessThan)))' || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".lessThan))'                                                                                                   || FunckySimpleType.BOOLEAN
        '"funcky:types".type ("funcky:commons".lessThan 0)'                                                                                                                                             || new FunckyFunctionType(FunckySimpleType.NUMBER, FunckySimpleType.BOOLEAN)
        '"funcky:types".type ("funcky:commons".lessThan \'a\')'                                                                                                                                         || new FunckyFunctionType(FunckySimpleType.CHARACTER, FunckySimpleType.BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:commons".lessThan ("funcky:commons".error "foo")))'                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".lessThan 0 0'                                                                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".lessThan 0 1'                                                                                                                                                                 || FunckyBoolean.TRUE
        '"funcky:commons".lessThan 1 0'                                                                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".lessThan 1 1'                                                                                                                                                                 || FunckyBoolean.FALSE
        '"funcky:commons".lessThan \'a\' \'a\''                                                                                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".lessThan \'a\' \'b\''                                                                                                                                                         || FunckyBoolean.TRUE
        '"funcky:commons".lessThan \'b\' \'a\''                                                                                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".lessThan \'b\' \'b\''                                                                                                                                                         || FunckyBoolean.FALSE
    }

    @Unroll('Test greaterEqual (expression: #expression)')
    def 'Test greaterEqual'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                              || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:commons".greaterEqual)'                                                                                                                          || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".greaterEqual))'                                                                                               || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".greaterEqual)))'                                                                        || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:commons".greaterEqual)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".greaterEqual)))' || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".greaterEqual))'                                                                                                       || FunckySimpleType.BOOLEAN
        '"funcky:types".type ("funcky:commons".greaterEqual 0)'                                                                                                                                                 || new FunckyFunctionType(FunckySimpleType.NUMBER, FunckySimpleType.BOOLEAN)
        '"funcky:types".type ("funcky:commons".greaterEqual \'a\')'                                                                                                                                             || new FunckyFunctionType(FunckySimpleType.CHARACTER, FunckySimpleType.BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:commons".greaterEqual ("funcky:commons".error "foo")))'                                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".greaterEqual 0 0'                                                                                                                                                                     || FunckyBoolean.TRUE
        '"funcky:commons".greaterEqual 0 1'                                                                                                                                                                     || FunckyBoolean.FALSE
        '"funcky:commons".greaterEqual 1 0'                                                                                                                                                                     || FunckyBoolean.TRUE
        '"funcky:commons".greaterEqual 1 1'                                                                                                                                                                     || FunckyBoolean.TRUE
        '"funcky:commons".greaterEqual \'a\' \'a\''                                                                                                                                                             || FunckyBoolean.TRUE
        '"funcky:commons".greaterEqual \'a\' \'b\''                                                                                                                                                             || FunckyBoolean.FALSE
        '"funcky:commons".greaterEqual \'b\' \'a\''                                                                                                                                                             || FunckyBoolean.TRUE
        '"funcky:commons".greaterEqual \'b\' \'b\''                                                                                                                                                             || FunckyBoolean.TRUE
    }

    @Unroll('Test lessEqual (expression: #expression)')
    def 'Test lessEqual'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                        || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:commons".lessEqual)'                                                                                                                       || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".lessEqual))'                                                                                            || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".lessEqual)))'                                                                     || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:commons".lessEqual)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".lessEqual)))' || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".lessEqual))'                                                                                                    || FunckySimpleType.BOOLEAN
        '"funcky:types".type ("funcky:commons".lessEqual 0)'                                                                                                                                              || new FunckyFunctionType(FunckySimpleType.NUMBER, FunckySimpleType.BOOLEAN)
        '"funcky:types".type ("funcky:commons".lessEqual \'a\')'                                                                                                                                          || new FunckyFunctionType(FunckySimpleType.CHARACTER, FunckySimpleType.BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:commons".lessEqual ("funcky:commons".error "foo")))'                                                                                      || FunckyBoolean.FALSE
        '"funcky:commons".lessEqual 0 0'                                                                                                                                                                  || FunckyBoolean.TRUE
        '"funcky:commons".lessEqual 0 1'                                                                                                                                                                  || FunckyBoolean.TRUE
        '"funcky:commons".lessEqual 1 0'                                                                                                                                                                  || FunckyBoolean.FALSE
        '"funcky:commons".lessEqual 1 1'                                                                                                                                                                  || FunckyBoolean.TRUE
        '"funcky:commons".lessEqual \'a\' \'a\''                                                                                                                                                          || FunckyBoolean.TRUE
        '"funcky:commons".lessEqual \'a\' \'b\''                                                                                                                                                          || FunckyBoolean.TRUE
        '"funcky:commons".lessEqual \'b\' \'a\''                                                                                                                                                          || FunckyBoolean.FALSE
        '"funcky:commons".lessEqual \'b\' \'b\''                                                                                                                                                          || FunckyBoolean.TRUE
    }

    @Unroll('Test compare (expression: #expression)')
    def 'Test compare'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                    || result
        '"funcky:commons".compare'                                                                                                                                                                    || Commons.COMPARE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".compare))'                                                                                          || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".compare)))'                                                                   || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:commons".compare)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".compare)))' || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".compare))'                                                                                                  || FunckySimpleType.NUMBER
        '"funcky:types".type ("funcky:commons".compare "funcky:types".Type)'                                                                                                                          || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.NUMBER)
        '"funcky:types".type ("funcky:commons".compare 0)'                                                                                                                                            || new FunckyFunctionType(FunckySimpleType.NUMBER, FunckySimpleType.NUMBER)
        '"funcky:commons".string ("funcky:commons".compare ("funcky:commons".error "foo"))'                                                                                                           || FunckyJavaConverter.convert('"funcky:commons".compare ("funcky:commons".error "foo")')
        '"funcky:commons".compare "funcky:types".Type "funcky:types".Type'                                                                                                                            || new FunckyNumber(0.0G)
        '"funcky:commons".compare "funcky:types".Type "funcky:types".Number'                                                                                                                          || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Type "funcky:types".Boolean'                                                                                                                         || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Type "funcky:types".Character'                                                                                                                       || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Type ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                            || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Type ("funcky:types".Function "funcky:types".Type $_)'                                                                                               || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Type ("funcky:types".Function $_ "funcky:types".Number)'                                                                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Type ("funcky:types".Function $_ $_)'                                                                                                                || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Type ("funcky:types".List "funcky:types".Type)'                                                                                                      || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Type ("funcky:types".List $_)'                                                                                                                       || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Type "funcky:types".String'                                                                                                                          || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Type ("funcky:types".Record [])'                                                                                                                     || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Type ("funcky:types".Record ["funcky:types".Type])'                                                                                                  || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Type ("funcky:types".Record [$_])'                                                                                                                   || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Type "funcky:types".Unit'                                                                                                                            || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Type $_'                                                                                                                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Number "funcky:types".Type'                                                                                                                          || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Number "funcky:types".Number'                                                                                                                        || new FunckyNumber(0.0G)
        '"funcky:commons".compare "funcky:types".Number "funcky:types".Boolean'                                                                                                                       || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Number "funcky:types".Character'                                                                                                                     || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Number ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                          || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Number ("funcky:types".Function "funcky:types".Type $_)'                                                                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Number ("funcky:types".Function $_ "funcky:types".Number)'                                                                                           || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Number ("funcky:types".Function $_ $_)'                                                                                                              || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Number ("funcky:types".List "funcky:types".Type)'                                                                                                    || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Number ("funcky:types".List $_)'                                                                                                                     || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Number "funcky:types".String'                                                                                                                        || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Number ("funcky:types".Record [])'                                                                                                                   || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Number ("funcky:types".Record ["funcky:types".Type])'                                                                                                || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Number ("funcky:types".Record [$_])'                                                                                                                 || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Number "funcky:types".Unit'                                                                                                                          || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Number $_'                                                                                                                                           || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Boolean "funcky:types".Type'                                                                                                                         || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Boolean "funcky:types".Number'                                                                                                                       || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Boolean "funcky:types".Boolean'                                                                                                                      || new FunckyNumber(0.0G)
        '"funcky:commons".compare "funcky:types".Boolean "funcky:types".Character'                                                                                                                    || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Boolean ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                         || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Boolean ("funcky:types".Function "funcky:types".Type $_)'                                                                                            || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Boolean ("funcky:types".Function $_ "funcky:types".Number)'                                                                                          || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Boolean ("funcky:types".Function $_ $_)'                                                                                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Boolean ("funcky:types".List "funcky:types".Type)'                                                                                                   || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Boolean ("funcky:types".List $_)'                                                                                                                    || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Boolean "funcky:types".String'                                                                                                                       || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Boolean ("funcky:types".Record [])'                                                                                                                  || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Boolean ("funcky:types".Record ["funcky:types".Type])'                                                                                               || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Boolean ("funcky:types".Record [$_])'                                                                                                                || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Boolean "funcky:types".Unit'                                                                                                                         || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Boolean $_'                                                                                                                                          || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Character "funcky:types".Type'                                                                                                                       || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Character "funcky:types".Number'                                                                                                                     || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Character "funcky:types".Boolean'                                                                                                                    || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Character "funcky:types".Character'                                                                                                                  || new FunckyNumber(0.0G)
        '"funcky:commons".compare "funcky:types".Character ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                       || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Character ("funcky:types".Function "funcky:types".Type $_)'                                                                                          || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Character ("funcky:types".Function $_ "funcky:types".Number)'                                                                                        || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Character ("funcky:types".Function $_ $_)'                                                                                                           || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Character ("funcky:types".List "funcky:types".Type)'                                                                                                 || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Character ("funcky:types".List $_)'                                                                                                                  || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Character "funcky:types".String'                                                                                                                     || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Character ("funcky:types".Record [])'                                                                                                                || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Character ("funcky:types".Record ["funcky:types".Type])'                                                                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Character ("funcky:types".Record [$_])'                                                                                                              || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Character "funcky:types".Unit'                                                                                                                       || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Character $_'                                                                                                                                        || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Type'                                                                            || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Number'                                                                          || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Boolean'                                                                         || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Character'                                                                       || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                            || new FunckyNumber(0.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function "funcky:types".Type $_)'                                               || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number)'                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function $_ $_)'                                                                || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".List "funcky:types".Type)'                                                      || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".List $_)'                                                                       || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".String'                                                                          || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Record [])'                                                                     || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Record ["funcky:types".Type])'                                                  || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Record [$_])'                                                                   || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Unit'                                                                            || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type "funcky:types".Number) $_'                                                                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Type'                                                                                               || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Number'                                                                                             || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Boolean'                                                                                            || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Character'                                                                                          || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                               || new FunckyNumber(1.0G)
        '"funcky:types".type ("funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type $_))'                                            || FunckySimpleType.NUMBER
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ "funcky:types".Number)'                                                                || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ $_)'                                                                                   || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".List "funcky:types".Type)'                                                                         || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".List $_)'                                                                                          || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) "funcky:types".String'                                                                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Record [])'                                                                                        || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Record ["funcky:types".Type])'                                                                     || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Record [$_])'                                                                                      || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Unit'                                                                                               || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) $_'                                                                                                                || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Type'                                                                                             || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Number'                                                                                           || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Boolean'                                                                                          || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Character'                                                                                        || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                             || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function "funcky:types".Type $_)'                                                                || new FunckyNumber(1.0G)
        '"funcky:types".type ("funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number))'                                        || FunckySimpleType.NUMBER
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ $_)'                                                                                 || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".List "funcky:types".Type)'                                                                       || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".List $_)'                                                                                        || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".String'                                                                                           || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Record [])'                                                                                      || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Record ["funcky:types".Type])'                                                                   || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Record [$_])'                                                                                    || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Unit'                                                                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) $_'                                                                                                              || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ $_) "funcky:types".Type'                                                                                                                || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ $_) "funcky:types".Number'                                                                                                              || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ $_) "funcky:types".Boolean'                                                                                                             || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ $_) "funcky:types".Character'                                                                                                           || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ $_) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ $_) ("funcky:types".Function "funcky:types".Type $_)'                                                                                   || new FunckyNumber(1.0G)
        '"funcky:types".type ("funcky:commons".compare ("funcky:types".Function $_ $_) ("funcky:types".Function $_ "funcky:types".Number))'                                                           || FunckySimpleType.NUMBER
        '"funcky:types".type ("funcky:commons".compare ("funcky:types".Function $_ $_) ("funcky:types".Function $_ $_))'                                                                              || FunckySimpleType.NUMBER
        '"funcky:commons".compare ("funcky:types".Function $_ $_) ("funcky:types".List "funcky:types".Type)'                                                                                          || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ $_) ("funcky:types".List $_)'                                                                                                           || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ $_) "funcky:types".String'                                                                                                              || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ $_) ("funcky:types".Record [])'                                                                                                         || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ $_) ("funcky:types".Record ["funcky:types".Type])'                                                                                      || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ $_) ("funcky:types".Record [$_])'                                                                                                       || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ $_) "funcky:types".Unit'                                                                                                                || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Function $_ $_) $_'                                                                                                                                 || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) "funcky:types".Type'                                                                                                      || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) "funcky:types".Number'                                                                                                    || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) "funcky:types".Boolean'                                                                                                   || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) "funcky:types".Character'                                                                                                 || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                      || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) ("funcky:types".Function "funcky:types".Type $_)'                                                                         || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) ("funcky:types".Function $_ "funcky:types".Number)'                                                                       || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) ("funcky:types".Function $_ $_)'                                                                                          || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) ("funcky:types".List "funcky:types".Type)'                                                                                || new FunckyNumber(0.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) ("funcky:types".List $_)'                                                                                                 || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) "funcky:types".String'                                                                                                    || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) ("funcky:types".Record [])'                                                                                               || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) ("funcky:types".Record ["funcky:types".Type])'                                                                            || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) ("funcky:types".Record [$_])'                                                                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) "funcky:types".Unit'                                                                                                      || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".List "funcky:types".Type) $_'                                                                                                                       || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".List $_) "funcky:types".Type'                                                                                                                       || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List $_) "funcky:types".Number'                                                                                                                     || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List $_) "funcky:types".Boolean'                                                                                                                    || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List $_) "funcky:types".Character'                                                                                                                  || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List $_) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                       || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List $_) ("funcky:types".Function "funcky:types".Type $_)'                                                                                          || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List $_) ("funcky:types".Function $_ "funcky:types".Number)'                                                                                        || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List $_) ("funcky:types".Function $_ $_)'                                                                                                           || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List $_) ("funcky:types".List "funcky:types".Type)'                                                                                                 || new FunckyNumber(1.0G)
        '"funcky:types".type ("funcky:commons".compare ("funcky:types".List $_) ("funcky:types".List $_))'                                                                                            || FunckySimpleType.NUMBER
        '"funcky:commons".compare ("funcky:types".List $_) "funcky:types".String'                                                                                                                     || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".List $_) ("funcky:types".Record [])'                                                                                                                || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".List $_) ("funcky:types".Record ["funcky:types".Type])'                                                                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".List $_) ("funcky:types".Record [$_])'                                                                                                              || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".List $_) "funcky:types".Unit'                                                                                                                       || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".List $_) $_'                                                                                                                                        || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".String "funcky:types".Type'                                                                                                                          || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".String "funcky:types".Number'                                                                                                                        || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".String "funcky:types".Boolean'                                                                                                                       || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".String "funcky:types".Character'                                                                                                                     || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".String ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                          || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".String ("funcky:types".Function "funcky:types".Type $_)'                                                                                             || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".String ("funcky:types".Function $_ "funcky:types".Number)'                                                                                           || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".String ("funcky:types".Function $_ $_)'                                                                                                              || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".String ("funcky:types".List "funcky:types".Type)'                                                                                                    || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".String ("funcky:types".List $_)'                                                                                                                     || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".String "funcky:types".String'                                                                                                                        || new FunckyNumber(0.0G)
        '"funcky:commons".compare "funcky:types".String ("funcky:types".Record [])'                                                                                                                   || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".String ("funcky:types".Record ["funcky:types".Type])'                                                                                                || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".String ("funcky:types".Record [$_])'                                                                                                                 || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".String "funcky:types".Unit'                                                                                                                          || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".String $_'                                                                                                                                           || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Record []) "funcky:types".Type'                                                                                                                     || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record []) "funcky:types".Number'                                                                                                                   || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record []) "funcky:types".Boolean'                                                                                                                  || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record []) "funcky:types".Character'                                                                                                                || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record []) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                     || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record []) ("funcky:types".Function "funcky:types".Type $_)'                                                                                        || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record []) ("funcky:types".Function $_ "funcky:types".Number)'                                                                                      || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record []) ("funcky:types".Function $_ $_)'                                                                                                         || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record []) ("funcky:types".List "funcky:types".Type)'                                                                                               || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record []) ("funcky:types".List $_)'                                                                                                                || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record []) "funcky:types".String'                                                                                                                   || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record []) ("funcky:types".Record [])'                                                                                                              || new FunckyNumber(0.0G)
        '"funcky:commons".compare ("funcky:types".Record []) ("funcky:types".Record ["funcky:types".Type])'                                                                                           || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Record []) ("funcky:types".Record [$_])'                                                                                                            || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Record []) "funcky:types".Unit'                                                                                                                     || new FunckyNumber(0.0G)
        '"funcky:commons".compare ("funcky:types".Record []) $_'                                                                                                                                      || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Type'                                                                                                  || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Number'                                                                                                || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Boolean'                                                                                               || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Character'                                                                                             || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                  || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function "funcky:types".Type $_)'                                                                     || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function $_ "funcky:types".Number)'                                                                   || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function $_ $_)'                                                                                      || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".List "funcky:types".Type)'                                                                            || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".List $_)'                                                                                             || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) "funcky:types".String'                                                                                                || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record [])'                                                                                           || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record ["funcky:types".Type])'                                                                        || new FunckyNumber(0.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record [$_])'                                                                                         || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Unit'                                                                                                  || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record ["funcky:types".Type]) $_'                                                                                                                   || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:types".Record [$_]) "funcky:types".Type'                                                                                                                   || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record [$_]) "funcky:types".Number'                                                                                                                 || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record [$_]) "funcky:types".Boolean'                                                                                                                || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record [$_]) "funcky:types".Character'                                                                                                              || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record [$_]) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                   || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record [$_]) ("funcky:types".Function "funcky:types".Type $_)'                                                                                      || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record [$_]) ("funcky:types".Function $_ "funcky:types".Number)'                                                                                    || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record [$_]) ("funcky:types".Function $_ $_)'                                                                                                       || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record [$_]) ("funcky:types".List "funcky:types".Type)'                                                                                             || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record [$_]) ("funcky:types".List $_)'                                                                                                              || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record [$_]) "funcky:types".String'                                                                                                                 || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record [$_]) ("funcky:types".Record [])'                                                                                                            || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record [$_]) ("funcky:types".Record ["funcky:types".Type])'                                                                                         || new FunckyNumber(1.0G)
        '"funcky:types".type ("funcky:commons".compare ("funcky:types".Record [$_]) ("funcky:types".Record [$_]))'                                                                                    || FunckySimpleType.NUMBER
        '"funcky:commons".compare ("funcky:types".Record [$_]) "funcky:types".Unit'                                                                                                                   || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:types".Record [$_]) $_'                                                                                                                                    || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Unit "funcky:types".Type'                                                                                                                            || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Unit "funcky:types".Number'                                                                                                                          || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Unit "funcky:types".Boolean'                                                                                                                         || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Unit "funcky:types".Character'                                                                                                                       || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Unit ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                            || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Unit ("funcky:types".Function "funcky:types".Type $_)'                                                                                               || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Unit ("funcky:types".Function $_ "funcky:types".Number)'                                                                                             || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Unit ("funcky:types".Function $_ $_)'                                                                                                                || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Unit ("funcky:types".List "funcky:types".Type)'                                                                                                      || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Unit ("funcky:types".List $_)'                                                                                                                       || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Unit "funcky:types".String'                                                                                                                          || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:types".Unit ("funcky:types".Record [])'                                                                                                                     || new FunckyNumber(0.0G)
        '"funcky:commons".compare "funcky:types".Unit ("funcky:types".Record ["funcky:types".Type])'                                                                                                  || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Unit ("funcky:types".Record [$_])'                                                                                                                   || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:types".Unit "funcky:types".Unit'                                                                                                                            || new FunckyNumber(0.0G)
        '"funcky:commons".compare "funcky:types".Unit $_'                                                                                                                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare $_ "funcky:types".Type'                                                                                                                                             || new FunckyNumber(1.0G)
        '"funcky:commons".compare $_ "funcky:types".Number'                                                                                                                                           || new FunckyNumber(1.0G)
        '"funcky:commons".compare $_ "funcky:types".Boolean'                                                                                                                                          || new FunckyNumber(1.0G)
        '"funcky:commons".compare $_ "funcky:types".Character'                                                                                                                                        || new FunckyNumber(1.0G)
        '"funcky:commons".compare $_ ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                                             || new FunckyNumber(1.0G)
        '"funcky:commons".compare $_ ("funcky:types".Function "funcky:types".Type $_)'                                                                                                                || new FunckyNumber(1.0G)
        '"funcky:commons".compare $_ ("funcky:types".Function $_ "funcky:types".Number)'                                                                                                              || new FunckyNumber(1.0G)
        '"funcky:commons".compare $_ ("funcky:types".Function $_ $_)'                                                                                                                                 || new FunckyNumber(1.0G)
        '"funcky:commons".compare $_ ("funcky:types".List "funcky:types".Type)'                                                                                                                       || new FunckyNumber(1.0G)
        '"funcky:commons".compare $_ ("funcky:types".List $_)'                                                                                                                                        || new FunckyNumber(1.0G)
        '"funcky:commons".compare $_ "funcky:types".String'                                                                                                                                           || new FunckyNumber(1.0G)
        '"funcky:commons".compare $_ ("funcky:types".Record [])'                                                                                                                                      || new FunckyNumber(1.0G)
        '"funcky:commons".compare $_ ("funcky:types".Record ["funcky:types".Type])'                                                                                                                   || new FunckyNumber(1.0G)
        '"funcky:commons".compare $_ ("funcky:types".Record [$_])'                                                                                                                                    || new FunckyNumber(1.0G)
        '"funcky:commons".compare $_ "funcky:types".Unit'                                                                                                                                             || new FunckyNumber(1.0G)
        '"funcky:types".type ("funcky:commons".compare $_ $_)'                                                                                                                                        || FunckySimpleType.NUMBER
        '"funcky:commons".compare 0 0'                                                                                                                                                                || new FunckyNumber(0.0G)
        '"funcky:commons".compare 0 1'                                                                                                                                                                || new FunckyNumber(-1.0G)
        '"funcky:commons".compare 1 0'                                                                                                                                                                || new FunckyNumber(1.0G)
        '"funcky:commons".compare 1 1'                                                                                                                                                                || new FunckyNumber(0.0G)
        '"funcky:commons".compare "funcky:booleans".false "funcky:booleans".false'                                                                                                                    || new FunckyNumber(0.0G)
        '"funcky:commons".compare "funcky:booleans".false "funcky:booleans".true'                                                                                                                     || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:booleans".true "funcky:booleans".false'                                                                                                                     || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:booleans".true "funcky:booleans".true'                                                                                                                      || new FunckyNumber(0.0G)
        '"funcky:commons".compare \'a\' \'a\''                                                                                                                                                        || new FunckyNumber(0.0G)
        '"funcky:commons".compare \'a\' \'b\''                                                                                                                                                        || new FunckyNumber(-1.0G)
        '"funcky:commons".compare \'b\' \'a\''                                                                                                                                                        || new FunckyNumber(1.0G)
        '"funcky:commons".compare \'b\' \'b\''                                                                                                                                                        || new FunckyNumber(0.0G)
        '"funcky:commons".compare "funcky:numbers".add "funcky:numbers".add'                                                                                                                          || new FunckyNumber(0.0G)
        '"funcky:commons".compare "funcky:numbers".add "funcky:numbers".subtract'                                                                                                                     || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "funcky:numbers".subtract "funcky:numbers".add'                                                                                                                     || new FunckyNumber(1.0G)
        '"funcky:commons".compare "funcky:numbers".subtract "funcky:numbers".subtract'                                                                                                                || new FunckyNumber(0.0G)
        '"funcky:commons".compare ("funcky:numbers".add 0) ("funcky:numbers".add 0)'                                                                                                                  || new FunckyNumber(0.0G)
        '"funcky:commons".compare ("funcky:numbers".add 0) ("funcky:numbers".add 1)'                                                                                                                  || new FunckyNumber(-1.0G)
        '"funcky:commons".compare ("funcky:numbers".add 1) ("funcky:numbers".add 0)'                                                                                                                  || new FunckyNumber(1.0G)
        '"funcky:commons".compare ("funcky:numbers".add 1) ("funcky:numbers".add 1)'                                                                                                                  || new FunckyNumber(0.0G)
        '"funcky:commons".compare [] []'                                                                                                                                                              || new FunckyNumber(0.0G)
        '"funcky:commons".compare [] [0]'                                                                                                                                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare [] [0, 1]'                                                                                                                                                          || new FunckyNumber(-1.0G)
        '"funcky:commons".compare [0] []'                                                                                                                                                             || new FunckyNumber(1.0G)
        '"funcky:commons".compare [0] [0]'                                                                                                                                                            || new FunckyNumber(0.0G)
        '"funcky:commons".compare [0] [0, 1]'                                                                                                                                                         || new FunckyNumber(-1.0G)
        '"funcky:commons".compare [0, 1] []'                                                                                                                                                          || new FunckyNumber(1.0G)
        '"funcky:commons".compare [0, 1] [0]'                                                                                                                                                         || new FunckyNumber(1.0G)
        '"funcky:commons".compare [0, 1] [0, 1]'                                                                                                                                                      || new FunckyNumber(0.0G)
        '"funcky:commons".compare "" ""'                                                                                                                                                              || new FunckyNumber(0.0G)
        '"funcky:commons".compare "" "a"'                                                                                                                                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "" "b"'                                                                                                                                                             || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "a" ""'                                                                                                                                                             || new FunckyNumber(1.0G)
        '"funcky:commons".compare "a" "a"'                                                                                                                                                            || new FunckyNumber(0.0G)
        '"funcky:commons".compare "a" "b"'                                                                                                                                                            || new FunckyNumber(-1.0G)
        '"funcky:commons".compare "b" ""'                                                                                                                                                             || new FunckyNumber(1.0G)
        '"funcky:commons".compare "b" "a"'                                                                                                                                                            || new FunckyNumber(1.0G)
        '"funcky:commons".compare "b" "b"'                                                                                                                                                            || new FunckyNumber(0.0G)
        '"funcky:commons".compare {} {}'                                                                                                                                                              || new FunckyNumber(0.0G)
        '"funcky:commons".compare {0} {0}'                                                                                                                                                            || new FunckyNumber(0.0G)
        '"funcky:commons".compare {0} {1}'                                                                                                                                                            || new FunckyNumber(-1.0G)
        '"funcky:commons".compare {1} {0}'                                                                                                                                                            || new FunckyNumber(1.0G)
        '"funcky:commons".compare {1} {1}'                                                                                                                                                            || new FunckyNumber(0.0G)
        '"funcky:commons".compare {0, \'a\'} {0, \'a\'}'                                                                                                                                              || new FunckyNumber(0.0G)
        '"funcky:commons".compare {0, \'a\'} {0, \'b\'}'                                                                                                                                              || new FunckyNumber(-1.0G)
        '"funcky:commons".compare {0, \'a\'} {1, \'a\'}'                                                                                                                                              || new FunckyNumber(-1.0G)
        '"funcky:commons".compare {0, \'a\'} {1, \'b\'}'                                                                                                                                              || new FunckyNumber(-1.0G)
        '"funcky:commons".compare {0, \'b\'} {0, \'a\'}'                                                                                                                                              || new FunckyNumber(1.0G)
        '"funcky:commons".compare {0, \'b\'} {0, \'b\'}'                                                                                                                                              || new FunckyNumber(0.0G)
        '"funcky:commons".compare {0, \'b\'} {1, \'a\'}'                                                                                                                                              || new FunckyNumber(-1.0G)
        '"funcky:commons".compare {0, \'b\'} {1, \'b\'}'                                                                                                                                              || new FunckyNumber(-1.0G)
        '"funcky:commons".compare {1, \'a\'} {0, \'a\'}'                                                                                                                                              || new FunckyNumber(1.0G)
        '"funcky:commons".compare {1, \'a\'} {0, \'b\'}'                                                                                                                                              || new FunckyNumber(1.0G)
        '"funcky:commons".compare {1, \'a\'} {1, \'a\'}'                                                                                                                                              || new FunckyNumber(0.0G)
        '"funcky:commons".compare {1, \'a\'} {1, \'b\'}'                                                                                                                                              || new FunckyNumber(-1.0G)
        '"funcky:commons".compare {1, \'b\'} {0, \'a\'}'                                                                                                                                              || new FunckyNumber(1.0G)
        '"funcky:commons".compare {1, \'b\'} {0, \'b\'}'                                                                                                                                              || new FunckyNumber(1.0G)
        '"funcky:commons".compare {1, \'b\'} {1, \'a\'}'                                                                                                                                              || new FunckyNumber(1.0G)
        '"funcky:commons".compare {1, \'b\'} {1, \'b\'}'                                                                                                                                              || new FunckyNumber(0.0G)
    }

    @Unroll('Test hash (expression: #expression)')
    def 'Test hash'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                       || result
        '"funcky:commons".hash'                                                                                                                                                                                          || Commons.HASH
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".hash))'                                                                                                                || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".type "funcky:commons".hash)'                                                                                                                                               || FunckySimpleType.NUMBER
        '"funcky:commons".equal ("funcky:commons".hash "funcky:types".Type) ("funcky:commons".hash "funcky:types".Type)'                                                                                                 || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:types".Number) ("funcky:commons".hash "funcky:types".Number)'                                                                                             || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:types".Boolean) ("funcky:commons".hash "funcky:types".Boolean)'                                                                                           || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:types".Character) ("funcky:commons".hash "funcky:types".Character)'                                                                                       || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".Function "funcky:types".Type "funcky:types".Number)) ("funcky:commons".hash ("funcky:types".Function "funcky:types".Type "funcky:types".Number))' || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".Function "funcky:types".Type $_)) ("funcky:commons".hash ("funcky:types".Function "funcky:types".Type $_))'                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".Function $_ "funcky:types".Number)) ("funcky:commons".hash ("funcky:types".Function $_ "funcky:types".Number))'                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".Function $_ $_)) ("funcky:commons".hash ("funcky:types".Function $_ $_))'                                                                         || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".List "funcky:types".Type)) ("funcky:commons".hash ("funcky:types".List "funcky:types".Type))'                                                     || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".List $_)) ("funcky:commons".hash ("funcky:types".List $_))'                                                                                       || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:types".String) ("funcky:commons".hash "funcky:types".String)'                                                                                             || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".Record [])) ("funcky:commons".hash ("funcky:types".Record []))'                                                                                   || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".Record ["funcky:types".Type])) ("funcky:commons".hash ("funcky:types".Record ["funcky:types".Type]))'                                             || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".Record [$_])) ("funcky:commons".hash ("funcky:types".Record [$_]))'                                                                               || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:types".Unit) ("funcky:commons".hash "funcky:types".Unit)'                                                                                                 || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash $_) ("funcky:commons".hash $_)'                                                                                                                                   || FunckyBoolean.FALSE
        '"funcky:commons".equal ("funcky:commons".hash 0) ("funcky:commons".hash 0)'                                                                                                                                     || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash 1) ("funcky:commons".hash 1)'                                                                                                                                     || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:booleans".false) ("funcky:commons".hash "funcky:booleans".false)'                                                                                         || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:booleans".true) ("funcky:commons".hash "funcky:booleans".true)'                                                                                           || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash \'a\') ("funcky:commons".hash \'a\')'                                                                                                                             || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash \'b\') ("funcky:commons".hash \'b\')'                                                                                                                             || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:numbers".add) ("funcky:commons".hash "funcky:numbers".add)'                                                                                               || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:booleans".and) ("funcky:commons".hash "funcky:booleans".and)'                                                                                             || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:numbers".add 0)) ("funcky:commons".hash ("funcky:numbers".add 0))'                                                                                       || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:booleans".and "funcky:booleans".false)) ("funcky:commons".hash ("funcky:booleans".and "funcky:booleans".false))'                                         || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash []) ("funcky:commons".hash [])'                                                                                                                                   || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash [0]) ("funcky:commons".hash [0])'                                                                                                                                 || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash [0, 1]) ("funcky:commons".hash [0, 1])'                                                                                                                           || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash "") ("funcky:commons".hash "")'                                                                                                                                   || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash "foo") ("funcky:commons".hash "foo")'                                                                                                                             || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash {}) ("funcky:commons".hash {})'                                                                                                                                   || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash {0}) ("funcky:commons".hash {0})'                                                                                                                                 || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:commons".hash {0, \'a\'}) ("funcky:commons".hash {0, \'a\'})'                                                                                                                   || FunckyBoolean.TRUE
    }

    @Unroll('Test if (expression: #expression)')
    def 'Test if'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                        || result
        '"funcky:commons".if'                                                                                                                                                                                                             || Commons.IF
        '"funcky:types".domain ("funcky:types".type "funcky:commons".if)'                                                                                                                                                                 || FunckySimpleType.BOOLEAN
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".if)))'                                                                                                            || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".if))))'                                                                                     || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".if))))'                                                                                      || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".if))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".if))))' || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".if))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".if))))'  || FunckyBoolean.TRUE
        '"funcky:commons".string ("funcky:commons".if ("funcky:commons".error "foo"))'                                                                                                                                                    || FunckyJavaConverter.convert('"funcky:commons".if ("funcky:commons".error "foo")')
        '"funcky:types".type ("funcky:commons".if "funcky:booleans".false 1)'                                                                                                                                                             || new FunckyFunctionType(FunckySimpleType.NUMBER, FunckySimpleType.NUMBER)
        '"funcky:types".type ("funcky:commons".if "funcky:booleans".false \'a\')'                                                                                                                                                         || new FunckyFunctionType(FunckySimpleType.CHARACTER, FunckySimpleType.CHARACTER)
        '"funcky:commons".string ("funcky:commons".if "funcky:booleans".false ("funcky:commons".error "foo"))'                                                                                                                            || FunckyJavaConverter.convert('"funcky:commons".if "funcky:booleans".false ("funcky:commons".error "foo")')
        '"funcky:commons".if "funcky:booleans".false 0 1'                                                                                                                                                                                 || new FunckyNumber(1.0G)
        '"funcky:commons".if "funcky:booleans".true 0 1'                                                                                                                                                                                  || new FunckyNumber(0.0G)
        '"funcky:commons".if "funcky:booleans".false \'a\' \'b\''                                                                                                                                                                         || new FunckyCharacter('b' as char)
        '"funcky:commons".if "funcky:booleans".true \'a\' \'b\''                                                                                                                                                                          || new FunckyCharacter('a' as char)
        '"funcky:commons".if "funcky:booleans".false ("funcky:commons".error "foo") 1'                                                                                                                                                    || new FunckyNumber(1.0G)
        '"funcky:commons".if "funcky:booleans".true 0 ("funcky:commons".error "foo")'                                                                                                                                                     || new FunckyNumber(0.0G)
        '"funcky:commons".if "funcky:booleans".false ("funcky:commons".error "foo") \'b\''                                                                                                                                                || new FunckyCharacter('b' as char)
        '"funcky:commons".if "funcky:booleans".true \'a\' ("funcky:commons".error "foo")'                                                                                                                                                 || new FunckyCharacter('a' as char)
    }

    @Unroll('Test string (expression: #expression)')
    def 'Test string'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                          || result
        '"funcky:commons".string'                                                                           || Commons.STRING
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".string))' || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".type "funcky:commons".string)'                                || FunckyListType.STRING
        '"funcky:commons".string "funcky:types".Type'                                                       || FunckyJavaConverter.convert('"funcky:types".Type')
        '"funcky:commons".string "funcky:types".Number'                                                     || FunckyJavaConverter.convert('"funcky:types".Number')
        '"funcky:commons".string "funcky:types".Boolean'                                                    || FunckyJavaConverter.convert('"funcky:types".Boolean')
        '"funcky:commons".string "funcky:types".Character'                                                  || FunckyJavaConverter.convert('"funcky:types".Character')
        '"funcky:commons".string ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'       || FunckyJavaConverter.convert('"funcky:types".Function "funcky:types".Type "funcky:types".Number')
        '"funcky:commons".string ("funcky:types".List "funcky:types".Type)'                                 || FunckyJavaConverter.convert('"funcky:types".List "funcky:types".Type')
        '"funcky:commons".string "funcky:types".String'                                                     || FunckyJavaConverter.convert('"funcky:types".List "funcky:types".Character')
        '"funcky:commons".string ("funcky:types".Record [])'                                                || FunckyJavaConverter.convert('"funcky:types".Record []')
        '"funcky:commons".string ("funcky:types".Record ["funcky:types".Type])'                             || FunckyJavaConverter.convert('"funcky:types".Record ["funcky:types".Type]')
        '"funcky:commons".string "funcky:types".Unit'                                                       || FunckyJavaConverter.convert('"funcky:types".Record []')
        '"funcky:lists".empty ("funcky:commons".string $_)'                                                 || FunckyBoolean.FALSE
        '"funcky:commons".string 0'                                                                         || FunckyJavaConverter.convert('0')
        '"funcky:commons".string 1'                                                                         || FunckyJavaConverter.convert('1')
        '"funcky:commons".string "funcky:booleans".false'                                                   || FunckyJavaConverter.convert('"funcky:booleans".false')
        '"funcky:commons".string "funcky:booleans".true'                                                    || FunckyJavaConverter.convert('"funcky:booleans".true')
        '"funcky:commons".string \'a\''                                                                     || FunckyJavaConverter.convert('a')
        '"funcky:commons".string \'b\''                                                                     || FunckyJavaConverter.convert('b')
        '"funcky:commons".string "funcky:types".type'                                                       || FunckyJavaConverter.convert('"funcky:types".type')
        '"funcky:commons".string ("funcky:numbers".add 0)'                                                  || FunckyJavaConverter.convert('"funcky:numbers".add 0')
        '"funcky:commons".string ("funcky:numbers".add ("funcky:commons".error "foo"))'                     || FunckyJavaConverter.convert('"funcky:numbers".add ("funcky:commons".error "foo")')
        '"funcky:commons".string []'                                                                        || FunckyJavaConverter.convert('[]')
        '"funcky:commons".string [0]'                                                                       || FunckyJavaConverter.convert('[0]')
        '"funcky:commons".string [0, 1]'                                                                    || FunckyJavaConverter.convert('[0, 1]')
        '"funcky:commons".string ["funcky:numbers".add 1 2]'                                                || FunckyJavaConverter.convert('[3]')
        '"funcky:commons".string ""'                                                                        || FunckyJavaConverter.convert('')
        '"funcky:commons".string "foo"'                                                                     || FunckyJavaConverter.convert('foo')
        '"funcky:commons".string {}'                                                                        || FunckyJavaConverter.convert('{}')
        '"funcky:commons".string {0}'                                                                       || FunckyJavaConverter.convert('{0}')
        '"funcky:commons".string {0, \'a\'}'                                                                || FunckyJavaConverter.convert('{0, a}')
    }

    @Unroll('Test number (expression: #expression)')
    def 'Test number'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                    || result
        '"funcky:commons".number'                     || Commons.NUMBER
        '"funcky:types".type "funcky:commons".number' || new FunckyFunctionType(FunckyListType.STRING, FunckySimpleType.NUMBER)
        '"funcky:commons".number "0.0"'               || new FunckyNumber(0.0G)
        '"funcky:commons".number "1.1"'               || new FunckyNumber(1.1G)
        '"funcky:commons".number "01.01"'             || new FunckyNumber(1.01G)
        '"funcky:commons".number "10.10"'             || new FunckyNumber(10.1G)
        '"funcky:commons".number ".0"'                || new FunckyNumber(0.0G)
        '"funcky:commons".number ".1"'                || new FunckyNumber(0.1G)
        '"funcky:commons".number ".01"'               || new FunckyNumber(0.01G)
        '"funcky:commons".number ".10"'               || new FunckyNumber(0.1G)
        '"funcky:commons".number "0"'                 || new FunckyNumber(0.0G)
        '"funcky:commons".number "1"'                 || new FunckyNumber(1.0G)
        '"funcky:commons".number "01"'                || new FunckyNumber(1.0G)
        '"funcky:commons".number "10"'                || new FunckyNumber(10.0G)
        '"funcky:commons".number "0.0e0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number "1.1e1"'             || new FunckyNumber(11.0G)
        '"funcky:commons".number "01.01e01"'          || new FunckyNumber(10.1G)
        '"funcky:commons".number "10.10e10"'          || new FunckyNumber(101000000000.0G)
        '"funcky:commons".number ".0e0"'              || new FunckyNumber(0.0G)
        '"funcky:commons".number ".1e1"'              || new FunckyNumber(1.0G)
        '"funcky:commons".number ".01e01"'            || new FunckyNumber(0.1G)
        '"funcky:commons".number ".10e10"'            || new FunckyNumber(1000000000.0G)
        '"funcky:commons".number "0e0"'               || new FunckyNumber(0.0G)
        '"funcky:commons".number "1e1"'               || new FunckyNumber(10.0G)
        '"funcky:commons".number "01e01"'             || new FunckyNumber(10.0G)
        '"funcky:commons".number "10e10"'             || new FunckyNumber(100000000000.0G)
        '"funcky:commons".number "0.0e+0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "1.1e+1"'            || new FunckyNumber(11.0G)
        '"funcky:commons".number "01.01e+01"'         || new FunckyNumber(10.1G)
        '"funcky:commons".number "10.10e+10"'         || new FunckyNumber(101000000000.0G)
        '"funcky:commons".number ".0e+0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number ".1e+1"'             || new FunckyNumber(1.0G)
        '"funcky:commons".number ".01e+01"'           || new FunckyNumber(0.1G)
        '"funcky:commons".number ".10e+10"'           || new FunckyNumber(1000000000.0G)
        '"funcky:commons".number "0e+0"'              || new FunckyNumber(0.0G)
        '"funcky:commons".number "1e+1"'              || new FunckyNumber(10.0G)
        '"funcky:commons".number "01e+01"'            || new FunckyNumber(10.0G)
        '"funcky:commons".number "10e+10"'            || new FunckyNumber(100000000000.0G)
        '"funcky:commons".number "0.0e-0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "1.1e-1"'            || new FunckyNumber(0.11G)
        '"funcky:commons".number "01.01e-01"'         || new FunckyNumber(0.101G)
        '"funcky:commons".number "10.10e-10"'         || new FunckyNumber(0.00000000101G)
        '"funcky:commons".number ".0e-0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number ".1e-1"'             || new FunckyNumber(0.01G)
        '"funcky:commons".number ".01e-01"'           || new FunckyNumber(0.001G)
        '"funcky:commons".number ".10e-10"'           || new FunckyNumber(0.00000000001G)
        '"funcky:commons".number "0e-0"'              || new FunckyNumber(0.0G)
        '"funcky:commons".number "1e-1"'              || new FunckyNumber(0.1G)
        '"funcky:commons".number "01e-01"'            || new FunckyNumber(0.1G)
        '"funcky:commons".number "10e-10"'            || new FunckyNumber(0.000000001G)
        '"funcky:commons".number "0.0E0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number "1.1E1"'             || new FunckyNumber(11.0G)
        '"funcky:commons".number "01.01E01"'          || new FunckyNumber(10.1G)
        '"funcky:commons".number "10.10E10"'          || new FunckyNumber(101000000000.0G)
        '"funcky:commons".number ".0E0"'              || new FunckyNumber(0.0G)
        '"funcky:commons".number ".1E1"'              || new FunckyNumber(1.0G)
        '"funcky:commons".number ".01E01"'            || new FunckyNumber(0.1G)
        '"funcky:commons".number ".10E10"'            || new FunckyNumber(1000000000.0G)
        '"funcky:commons".number "0E0"'               || new FunckyNumber(0.0G)
        '"funcky:commons".number "1E1"'               || new FunckyNumber(10.0G)
        '"funcky:commons".number "01E01"'             || new FunckyNumber(10.0G)
        '"funcky:commons".number "10E10"'             || new FunckyNumber(100000000000.0G)
        '"funcky:commons".number "0.0E+0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "1.1E+1"'            || new FunckyNumber(11.0G)
        '"funcky:commons".number "01.01E+01"'         || new FunckyNumber(10.1G)
        '"funcky:commons".number "10.10E+10"'         || new FunckyNumber(101000000000.0G)
        '"funcky:commons".number ".0E+0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number ".1E+1"'             || new FunckyNumber(1.0G)
        '"funcky:commons".number ".01E+01"'           || new FunckyNumber(0.1G)
        '"funcky:commons".number ".10E+10"'           || new FunckyNumber(1000000000.0G)
        '"funcky:commons".number "0E+0"'              || new FunckyNumber(0.0G)
        '"funcky:commons".number "1E+1"'              || new FunckyNumber(10.0G)
        '"funcky:commons".number "01E+01"'            || new FunckyNumber(10.0G)
        '"funcky:commons".number "10E+10"'            || new FunckyNumber(100000000000.0G)
        '"funcky:commons".number "0.0E-0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "1.1E-1"'            || new FunckyNumber(0.11G)
        '"funcky:commons".number "01.01E-01"'         || new FunckyNumber(0.101G)
        '"funcky:commons".number "10.10E-10"'         || new FunckyNumber(0.00000000101G)
        '"funcky:commons".number ".0E-0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number ".1E-1"'             || new FunckyNumber(0.01G)
        '"funcky:commons".number ".01E-01"'           || new FunckyNumber(0.001G)
        '"funcky:commons".number ".10E-10"'           || new FunckyNumber(0.00000000001G)
        '"funcky:commons".number "0E-0"'              || new FunckyNumber(0.0G)
        '"funcky:commons".number "1E-1"'              || new FunckyNumber(0.1G)
        '"funcky:commons".number "01E-01"'            || new FunckyNumber(0.1G)
        '"funcky:commons".number "10E-10"'            || new FunckyNumber(0.000000001G)
        '"funcky:commons".number "+0.0"'              || new FunckyNumber(0.0G)
        '"funcky:commons".number "+1.1"'              || new FunckyNumber(1.1G)
        '"funcky:commons".number "+01.01"'            || new FunckyNumber(1.01G)
        '"funcky:commons".number "+10.10"'            || new FunckyNumber(10.1G)
        '"funcky:commons".number "+.0"'               || new FunckyNumber(0.0G)
        '"funcky:commons".number "+.1"'               || new FunckyNumber(0.1G)
        '"funcky:commons".number "+.01"'              || new FunckyNumber(0.01G)
        '"funcky:commons".number "+.10"'              || new FunckyNumber(0.1G)
        '"funcky:commons".number "+0"'                || new FunckyNumber(0.0G)
        '"funcky:commons".number "+1"'                || new FunckyNumber(1.0G)
        '"funcky:commons".number "+01"'               || new FunckyNumber(1.0G)
        '"funcky:commons".number "+10"'               || new FunckyNumber(10.0G)
        '"funcky:commons".number "+0.0e0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "+1.1e1"'            || new FunckyNumber(11.0G)
        '"funcky:commons".number "+01.01e01"'         || new FunckyNumber(10.1G)
        '"funcky:commons".number "+10.10e10"'         || new FunckyNumber(101000000000.0G)
        '"funcky:commons".number "+.0e0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number "+.1e1"'             || new FunckyNumber(1.0G)
        '"funcky:commons".number "+.01e01"'           || new FunckyNumber(0.1G)
        '"funcky:commons".number "+.10e10"'           || new FunckyNumber(1000000000.0G)
        '"funcky:commons".number "+0e0"'              || new FunckyNumber(0.0G)
        '"funcky:commons".number "+1e1"'              || new FunckyNumber(10.0G)
        '"funcky:commons".number "+01e01"'            || new FunckyNumber(10.0G)
        '"funcky:commons".number "+10e10"'            || new FunckyNumber(100000000000.0G)
        '"funcky:commons".number "+0.0e+0"'           || new FunckyNumber(0.0G)
        '"funcky:commons".number "+1.1e+1"'           || new FunckyNumber(11.0G)
        '"funcky:commons".number "+01.01e+01"'        || new FunckyNumber(10.1G)
        '"funcky:commons".number "+10.10e+10"'        || new FunckyNumber(101000000000.0G)
        '"funcky:commons".number "+.0e+0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "+.1e+1"'            || new FunckyNumber(1.0G)
        '"funcky:commons".number "+.01e+01"'          || new FunckyNumber(0.1G)
        '"funcky:commons".number "+.10e+10"'          || new FunckyNumber(1000000000.0G)
        '"funcky:commons".number "+0e+0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number "+1e+1"'             || new FunckyNumber(10.0G)
        '"funcky:commons".number "+01e+01"'           || new FunckyNumber(10.0G)
        '"funcky:commons".number "+10e+10"'           || new FunckyNumber(100000000000.0G)
        '"funcky:commons".number "+0.0e-0"'           || new FunckyNumber(0.0G)
        '"funcky:commons".number "+1.1e-1"'           || new FunckyNumber(0.11G)
        '"funcky:commons".number "+01.01e-01"'        || new FunckyNumber(0.101G)
        '"funcky:commons".number "+10.10e-10"'        || new FunckyNumber(0.00000000101G)
        '"funcky:commons".number "+.0e-0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "+.1e-1"'            || new FunckyNumber(0.01G)
        '"funcky:commons".number "+.01e-01"'          || new FunckyNumber(0.001G)
        '"funcky:commons".number "+.10e-10"'          || new FunckyNumber(0.00000000001G)
        '"funcky:commons".number "+0e-0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number "+1e-1"'             || new FunckyNumber(0.1G)
        '"funcky:commons".number "+01e-01"'           || new FunckyNumber(0.1G)
        '"funcky:commons".number "+10e-10"'           || new FunckyNumber(0.000000001G)
        '"funcky:commons".number "+0.0E0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "+1.1E1"'            || new FunckyNumber(11.0G)
        '"funcky:commons".number "+01.01E01"'         || new FunckyNumber(10.1G)
        '"funcky:commons".number "+10.10E10"'         || new FunckyNumber(101000000000.0G)
        '"funcky:commons".number "+.0E0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number "+.1E1"'             || new FunckyNumber(1.0G)
        '"funcky:commons".number "+.01E01"'           || new FunckyNumber(0.1G)
        '"funcky:commons".number "+.10E10"'           || new FunckyNumber(1000000000.0G)
        '"funcky:commons".number "+0E0"'              || new FunckyNumber(0.0G)
        '"funcky:commons".number "+1E1"'              || new FunckyNumber(10.0G)
        '"funcky:commons".number "+01E01"'            || new FunckyNumber(10.0G)
        '"funcky:commons".number "+10E10"'            || new FunckyNumber(100000000000.0G)
        '"funcky:commons".number "+0.0E+0"'           || new FunckyNumber(0.0G)
        '"funcky:commons".number "+1.1E+1"'           || new FunckyNumber(11.0G)
        '"funcky:commons".number "+01.01E+01"'        || new FunckyNumber(10.1G)
        '"funcky:commons".number "+10.10E+10"'        || new FunckyNumber(101000000000.0G)
        '"funcky:commons".number "+.0E+0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "+.1E+1"'            || new FunckyNumber(1.0G)
        '"funcky:commons".number "+.01E+01"'          || new FunckyNumber(0.1G)
        '"funcky:commons".number "+.10E+10"'          || new FunckyNumber(1000000000.0G)
        '"funcky:commons".number "+0E+0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number "+1E+1"'             || new FunckyNumber(10.0G)
        '"funcky:commons".number "+01E+01"'           || new FunckyNumber(10.0G)
        '"funcky:commons".number "+10E+10"'           || new FunckyNumber(100000000000.0G)
        '"funcky:commons".number "+0.0E-0"'           || new FunckyNumber(0.0G)
        '"funcky:commons".number "+1.1E-1"'           || new FunckyNumber(0.11G)
        '"funcky:commons".number "+01.01E-01"'        || new FunckyNumber(0.101G)
        '"funcky:commons".number "+10.10E-10"'        || new FunckyNumber(0.00000000101G)
        '"funcky:commons".number "+.0E-0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "+.1E-1"'            || new FunckyNumber(0.01G)
        '"funcky:commons".number "+.01E-01"'          || new FunckyNumber(0.001G)
        '"funcky:commons".number "+.10E-10"'          || new FunckyNumber(0.00000000001G)
        '"funcky:commons".number "+0E-0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number "+1E-1"'             || new FunckyNumber(0.1G)
        '"funcky:commons".number "+01E-01"'           || new FunckyNumber(0.1G)
        '"funcky:commons".number "+10E-10"'           || new FunckyNumber(0.000000001G)
        '"funcky:commons".number "-0.0"'              || new FunckyNumber(0.0G)
        '"funcky:commons".number "-1.1"'              || new FunckyNumber(-1.1G)
        '"funcky:commons".number "-01.01"'            || new FunckyNumber(-1.01G)
        '"funcky:commons".number "-10.10"'            || new FunckyNumber(-10.1G)
        '"funcky:commons".number "-.0"'               || new FunckyNumber(0.0G)
        '"funcky:commons".number "-.1"'               || new FunckyNumber(-0.1G)
        '"funcky:commons".number "-.01"'              || new FunckyNumber(-0.01G)
        '"funcky:commons".number "-.10"'              || new FunckyNumber(-0.1G)
        '"funcky:commons".number "-0"'                || new FunckyNumber(0.0G)
        '"funcky:commons".number "-1"'                || new FunckyNumber(-1.0G)
        '"funcky:commons".number "-01"'               || new FunckyNumber(-1.0G)
        '"funcky:commons".number "-10"'               || new FunckyNumber(-10.0G)
        '"funcky:commons".number "-0.0e0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "-1.1e1"'            || new FunckyNumber(-11.0G)
        '"funcky:commons".number "-01.01e01"'         || new FunckyNumber(-10.1G)
        '"funcky:commons".number "-10.10e10"'         || new FunckyNumber(-101000000000.0G)
        '"funcky:commons".number "-.0e0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number "-.1e1"'             || new FunckyNumber(-1.0G)
        '"funcky:commons".number "-.01e01"'           || new FunckyNumber(-0.1G)
        '"funcky:commons".number "-.10e10"'           || new FunckyNumber(-1000000000.0G)
        '"funcky:commons".number "-0e0"'              || new FunckyNumber(0.0G)
        '"funcky:commons".number "-1e1"'              || new FunckyNumber(-10.0G)
        '"funcky:commons".number "-01e01"'            || new FunckyNumber(-10.0G)
        '"funcky:commons".number "-10e10"'            || new FunckyNumber(-100000000000.0G)
        '"funcky:commons".number "-0.0e+0"'           || new FunckyNumber(0.0G)
        '"funcky:commons".number "-1.1e+1"'           || new FunckyNumber(-11.0G)
        '"funcky:commons".number "-01.01e+01"'        || new FunckyNumber(-10.1G)
        '"funcky:commons".number "-10.10e+10"'        || new FunckyNumber(-101000000000.0G)
        '"funcky:commons".number "-.0e+0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "-.1e+1"'            || new FunckyNumber(-1.0G)
        '"funcky:commons".number "-.01e+01"'          || new FunckyNumber(-0.1G)
        '"funcky:commons".number "-.10e+10"'          || new FunckyNumber(-1000000000.0G)
        '"funcky:commons".number "-0e+0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number "-1e+1"'             || new FunckyNumber(-10.0G)
        '"funcky:commons".number "-01e+01"'           || new FunckyNumber(-10.0G)
        '"funcky:commons".number "-10e+10"'           || new FunckyNumber(-100000000000.0G)
        '"funcky:commons".number "-0.0e-0"'           || new FunckyNumber(0.0G)
        '"funcky:commons".number "-1.1e-1"'           || new FunckyNumber(-0.11G)
        '"funcky:commons".number "-01.01e-01"'        || new FunckyNumber(-0.101G)
        '"funcky:commons".number "-10.10e-10"'        || new FunckyNumber(-0.00000000101G)
        '"funcky:commons".number "-.0e-0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "-.1e-1"'            || new FunckyNumber(-0.01G)
        '"funcky:commons".number "-.01e-01"'          || new FunckyNumber(-0.001G)
        '"funcky:commons".number "-.10e-10"'          || new FunckyNumber(-0.00000000001G)
        '"funcky:commons".number "-0e-0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number "-1e-1"'             || new FunckyNumber(-0.1G)
        '"funcky:commons".number "-01e-01"'           || new FunckyNumber(-0.1G)
        '"funcky:commons".number "-10e-10"'           || new FunckyNumber(-0.000000001G)
        '"funcky:commons".number "-0.0E0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "-1.1E1"'            || new FunckyNumber(-11.0G)
        '"funcky:commons".number "-01.01E01"'         || new FunckyNumber(-10.1G)
        '"funcky:commons".number "-10.10E10"'         || new FunckyNumber(-101000000000.0G)
        '"funcky:commons".number "-.0E0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number "-.1E1"'             || new FunckyNumber(-1.0G)
        '"funcky:commons".number "-.01E01"'           || new FunckyNumber(-0.1G)
        '"funcky:commons".number "-.10E10"'           || new FunckyNumber(-1000000000.0G)
        '"funcky:commons".number "-0E0"'              || new FunckyNumber(0.0G)
        '"funcky:commons".number "-1E1"'              || new FunckyNumber(-10.0G)
        '"funcky:commons".number "-01E01"'            || new FunckyNumber(-10.0G)
        '"funcky:commons".number "-10E10"'            || new FunckyNumber(-100000000000.0G)
        '"funcky:commons".number "-0.0E+0"'           || new FunckyNumber(0.0G)
        '"funcky:commons".number "-1.1E+1"'           || new FunckyNumber(-11.0G)
        '"funcky:commons".number "-01.01E+01"'        || new FunckyNumber(-10.1G)
        '"funcky:commons".number "-10.10E+10"'        || new FunckyNumber(-101000000000.0G)
        '"funcky:commons".number "-.0E+0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "-.1E+1"'            || new FunckyNumber(-1.0G)
        '"funcky:commons".number "-.01E+01"'          || new FunckyNumber(-0.1G)
        '"funcky:commons".number "-.10E+10"'          || new FunckyNumber(-1000000000.0G)
        '"funcky:commons".number "-0E+0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number "-1E+1"'             || new FunckyNumber(-10.0G)
        '"funcky:commons".number "-01E+01"'           || new FunckyNumber(-10.0G)
        '"funcky:commons".number "-10E+10"'           || new FunckyNumber(-100000000000.0G)
        '"funcky:commons".number "-0.0E-0"'           || new FunckyNumber(0.0G)
        '"funcky:commons".number "-1.1E-1"'           || new FunckyNumber(-0.11G)
        '"funcky:commons".number "-01.01E-01"'        || new FunckyNumber(-0.101G)
        '"funcky:commons".number "-10.10E-10"'        || new FunckyNumber(-0.00000000101G)
        '"funcky:commons".number "-.0E-0"'            || new FunckyNumber(0.0G)
        '"funcky:commons".number "-.1E-1"'            || new FunckyNumber(-0.01G)
        '"funcky:commons".number "-.01E-01"'          || new FunckyNumber(-0.001G)
        '"funcky:commons".number "-.10E-10"'          || new FunckyNumber(-0.00000000001G)
        '"funcky:commons".number "-0E-0"'             || new FunckyNumber(0.0G)
        '"funcky:commons".number "-1E-1"'             || new FunckyNumber(-0.1G)
        '"funcky:commons".number "-01E-01"'           || new FunckyNumber(-0.1G)
        '"funcky:commons".number "-10E-10"'           || new FunckyNumber(-0.000000001G)
    }

    @Unroll('Test number (runtime error, expression: #expression)')
    def 'Test number (runtime error)'(final String expression, final String message) {
        when:
        engine.eval(expression)
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(String.format(Commons.ERROR_INVALID_NUMBER, message))
        where:
        expression                      || message
        '"funcky:commons".number ""'    || ''
        '"funcky:commons".number "foo"' || 'foo'
    }

    @Unroll('Test error (expression: #expression)')
    def 'Test error'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                        || result
        '"funcky:commons".error'                                                                          || Commons.ERROR
        '"funcky:types".domain ("funcky:types".type "funcky:commons".error)'                              || FunckyListType.STRING
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".type "funcky:commons".error))' || FunckyBoolean.TRUE
    }

    @Unroll('Test error (runtime error, expression: #expression)')
    def 'Test error (runtime error)'(final String expression, final String message) {
        when:
        engine.eval(expression)
        then:
        final FunckyRuntimeException e = thrown()
        e.message == message
        where:
        expression                                              || message
        '"funcky:commons".error "foo"'                          || 'foo\r\n    in `"funcky:commons".error "foo"` in funcky:stdin at line 1 at column 1'
        '"funcky:commons".error "bar"'                          || 'bar\r\n    in `"funcky:commons".error "bar"` in funcky:stdin at line 1 at column 1'
        '"funcky:commons".error ("funcky:commons".error "foo")' || 'foo\r\n    in `"funcky:commons".error "foo"` in funcky:stdin at line 1 at column 25\r\n    in `"funcky:commons".error ("funcky:commons".error "foo")` in funcky:stdin at line 1 at column 1'
    }

    @Unroll('Test bottom (expression: #expression)')
    def 'Test bottom'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                          || result
        '"funcky:commons".bottom'                                                                                                                                           || Commons.BOTTOM
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".bottom))'                                                                 || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".type "funcky:commons".bottom))'                                                                  || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:commons".bottom)) ("funcky:types".range ("funcky:types".type "funcky:commons".bottom))' || FunckyBoolean.FALSE
    }
}
