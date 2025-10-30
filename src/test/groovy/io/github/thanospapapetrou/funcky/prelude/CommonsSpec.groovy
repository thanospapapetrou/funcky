package io.github.thanospapapetrou.funcky.prelude

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.FunckyJavaConverter
import io.github.thanospapapetrou.funcky.runtime.FunckyCharacter
import io.github.thanospapapetrou.funcky.runtime.FunckyNumber
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException
import io.github.thanospapapetrou.funcky.runtime.prelude.Commons
import spock.lang.Unroll

import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.FALSE
import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.TRUE
import static io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType.FUNCTION
import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.STRING
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.*

class CommonsSpec extends BaseSpec {
    @Unroll('Test equal (expression: #expression)')
    def 'Test equal'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                || result
        '"funcky:commons".equal'                                                                                                                                                                  || new Commons().$equal
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".equal))'                                                                                        || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".equal)))'                                                                 || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:commons".equal)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".equal)))' || TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".equal))'                                                                                                || BOOLEAN
        '"funcky:types".type ("funcky:commons".equal "funcky:types".Type)'                                                                                                                        || FUNCTION(TYPE, BOOLEAN)
        '"funcky:types".type ("funcky:commons".equal 0)'                                                                                                                                          || FUNCTION(NUMBER, BOOLEAN)
        '"funcky:commons".string ("funcky:commons".equal ("funcky:commons".error "foo"))'                                                                                                         || new FunckyJavaConverter().convert('"funcky:commons".equal ("funcky:commons".error "foo")')
        '"funcky:commons".equal "funcky:types".Type "funcky:types".Type'                                                                                                                          || TRUE
        '"funcky:commons".equal "funcky:types".Type "funcky:types".Number'                                                                                                                        || FALSE
        '"funcky:commons".equal "funcky:types".Type "funcky:types".Boolean'                                                                                                                       || FALSE
        '"funcky:commons".equal "funcky:types".Type "funcky:types".Character'                                                                                                                     || FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                          || FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".Function "funcky:types".Type $_)'                                                                                             || FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".Function $_ "funcky:types".Number)'                                                                                           || FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".Function $_ $_)'                                                                                                              || FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".List "funcky:types".Type)'                                                                                                    || FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".List $_)'                                                                                                                     || FALSE
        '"funcky:commons".equal "funcky:types".Type "funcky:types".String'                                                                                                                        || FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".Record [])'                                                                                                                   || FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".Record ["funcky:types".Type])'                                                                                                || FALSE
        '"funcky:commons".equal "funcky:types".Type ("funcky:types".Record [$_])'                                                                                                                 || FALSE
        '"funcky:commons".equal "funcky:types".Type "funcky:types".Unit'                                                                                                                          || FALSE
        '"funcky:commons".equal "funcky:types".Type $_'                                                                                                                                           || FALSE
        '"funcky:commons".equal "funcky:types".Number "funcky:types".Type'                                                                                                                        || FALSE
        '"funcky:commons".equal "funcky:types".Number "funcky:types".Number'                                                                                                                      || TRUE
        '"funcky:commons".equal "funcky:types".Number "funcky:types".Boolean'                                                                                                                     || FALSE
        '"funcky:commons".equal "funcky:types".Number "funcky:types".Character'                                                                                                                   || FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                        || FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".Function "funcky:types".Type $_)'                                                                                           || FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".Function $_ "funcky:types".Number)'                                                                                         || FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".Function $_ $_)'                                                                                                            || FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".List "funcky:types".Type)'                                                                                                  || FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".List $_)'                                                                                                                   || FALSE
        '"funcky:commons".equal "funcky:types".Number "funcky:types".String'                                                                                                                      || FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".Record [])'                                                                                                                 || FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".Record ["funcky:types".Type])'                                                                                              || FALSE
        '"funcky:commons".equal "funcky:types".Number ("funcky:types".Record [$_])'                                                                                                               || FALSE
        '"funcky:commons".equal "funcky:types".Number "funcky:types".Unit'                                                                                                                        || FALSE
        '"funcky:commons".equal "funcky:types".Number $_'                                                                                                                                         || FALSE
        '"funcky:commons".equal "funcky:types".Boolean "funcky:types".Type'                                                                                                                       || FALSE
        '"funcky:commons".equal "funcky:types".Boolean "funcky:types".Number'                                                                                                                     || FALSE
        '"funcky:commons".equal "funcky:types".Boolean "funcky:types".Boolean'                                                                                                                    || TRUE
        '"funcky:commons".equal "funcky:types".Boolean "funcky:types".Character'                                                                                                                  || FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                       || FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".Function "funcky:types".Type $_)'                                                                                          || FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".Function $_ "funcky:types".Number)'                                                                                        || FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".Function $_ $_)'                                                                                                           || FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".List "funcky:types".Type)'                                                                                                 || FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".List $_)'                                                                                                                  || FALSE
        '"funcky:commons".equal "funcky:types".Boolean "funcky:types".String'                                                                                                                     || FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".Record [])'                                                                                                                || FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".Record ["funcky:types".Type])'                                                                                             || FALSE
        '"funcky:commons".equal "funcky:types".Boolean ("funcky:types".Record [$_])'                                                                                                              || FALSE
        '"funcky:commons".equal "funcky:types".Boolean "funcky:types".Unit'                                                                                                                       || FALSE
        '"funcky:commons".equal "funcky:types".Boolean $_'                                                                                                                                        || FALSE
        '"funcky:commons".equal "funcky:types".Character "funcky:types".Type'                                                                                                                     || FALSE
        '"funcky:commons".equal "funcky:types".Character "funcky:types".Number'                                                                                                                   || FALSE
        '"funcky:commons".equal "funcky:types".Character "funcky:types".Boolean'                                                                                                                  || FALSE
        '"funcky:commons".equal "funcky:types".Character "funcky:types".Character'                                                                                                                || TRUE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                     || FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".Function "funcky:types".Type $_)'                                                                                        || FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".Function $_ "funcky:types".Number)'                                                                                      || FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".Function $_ $_)'                                                                                                         || FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".List "funcky:types".Type)'                                                                                               || FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".List $_)'                                                                                                                || FALSE
        '"funcky:commons".equal "funcky:types".Character "funcky:types".String'                                                                                                                   || FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".Record [])'                                                                                                              || FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".Record ["funcky:types".Type])'                                                                                           || FALSE
        '"funcky:commons".equal "funcky:types".Character ("funcky:types".Record [$_])'                                                                                                            || FALSE
        '"funcky:commons".equal "funcky:types".Character "funcky:types".Unit'                                                                                                                     || FALSE
        '"funcky:commons".equal "funcky:types".Character $_'                                                                                                                                      || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Type'                                                                          || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Number'                                                                        || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Boolean'                                                                       || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Character'                                                                     || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                          || TRUE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function "funcky:types".Type $_)'                                             || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number)'                                           || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function $_ $_)'                                                              || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".List "funcky:types".Type)'                                                    || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".List $_)'                                                                     || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".String'                                                                        || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Record [])'                                                                   || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Record ["funcky:types".Type])'                                                || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Record [$_])'                                                                 || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) "funcky:types".Unit'                                                                          || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type "funcky:types".Number) $_'                                                                                           || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Type'                                                                                             || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Number'                                                                                           || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Boolean'                                                                                          || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Character'                                                                                        || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                             || FALSE
        '"funcky:types".type ("funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type $_))'                                          || BOOLEAN
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ "funcky:types".Number)'                                                              || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ $_)'                                                                                 || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".List "funcky:types".Type)'                                                                       || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".List $_)'                                                                                        || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) "funcky:types".String'                                                                                           || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Record [])'                                                                                      || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Record ["funcky:types".Type])'                                                                   || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Record [$_])'                                                                                    || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) "funcky:types".Unit'                                                                                             || FALSE
        '"funcky:commons".equal ("funcky:types".Function "funcky:types".Type $_) $_'                                                                                                              || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Type'                                                                                           || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Number'                                                                                         || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Boolean'                                                                                        || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Character'                                                                                      || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                           || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function "funcky:types".Type $_)'                                                              || FALSE
        '"funcky:types".type ("funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number))'                                      || BOOLEAN
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ $_)'                                                                               || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".List "funcky:types".Type)'                                                                     || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".List $_)'                                                                                      || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".String'                                                                                         || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Record [])'                                                                                    || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Record ["funcky:types".Type])'                                                                 || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Record [$_])'                                                                                  || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) "funcky:types".Unit'                                                                                           || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ "funcky:types".Number) $_'                                                                                                            || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) "funcky:types".Type'                                                                                                              || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) "funcky:types".Number'                                                                                                            || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) "funcky:types".Boolean'                                                                                                           || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) "funcky:types".Character'                                                                                                         || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                              || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".Function "funcky:types".Type $_)'                                                                                 || FALSE
        '"funcky:types".type ("funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".Function $_ "funcky:types".Number))'                                                         || BOOLEAN
        '"funcky:types".type ("funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".Function $_ $_))'                                                                            || BOOLEAN
        '"funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".List "funcky:types".Type)'                                                                                        || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".List $_)'                                                                                                         || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) "funcky:types".String'                                                                                                            || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".Record [])'                                                                                                       || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".Record ["funcky:types".Type])'                                                                                    || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) ("funcky:types".Record [$_])'                                                                                                     || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) "funcky:types".Unit'                                                                                                              || FALSE
        '"funcky:commons".equal ("funcky:types".Function $_ $_) $_'                                                                                                                               || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) "funcky:types".Type'                                                                                                    || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) "funcky:types".Number'                                                                                                  || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) "funcky:types".Boolean'                                                                                                 || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) "funcky:types".Character'                                                                                               || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                    || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".Function "funcky:types".Type $_)'                                                                       || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".Function $_ "funcky:types".Number)'                                                                     || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".Function $_ $_)'                                                                                        || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".List "funcky:types".Type)'                                                                              || TRUE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".List $_)'                                                                                               || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) "funcky:types".String'                                                                                                  || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".Record [])'                                                                                             || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".Record ["funcky:types".Type])'                                                                          || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) ("funcky:types".Record [$_])'                                                                                           || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) "funcky:types".Unit'                                                                                                    || FALSE
        '"funcky:commons".equal ("funcky:types".List "funcky:types".Type) $_'                                                                                                                     || FALSE
        '"funcky:commons".equal ("funcky:types".List $_) "funcky:types".Type'                                                                                                                     || FALSE
        '"funcky:commons".equal ("funcky:types".List $_) "funcky:types".Number'                                                                                                                   || FALSE
        '"funcky:commons".equal ("funcky:types".List $_) "funcky:types".Boolean'                                                                                                                  || FALSE
        '"funcky:commons".equal ("funcky:types".List $_) "funcky:types".Character'                                                                                                                || FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                     || FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".Function "funcky:types".Type $_)'                                                                                        || FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".Function $_ "funcky:types".Number)'                                                                                      || FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".Function $_ $_)'                                                                                                         || FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".List "funcky:types".Type)'                                                                                               || FALSE
        '"funcky:types".type ("funcky:commons".equal ("funcky:types".List $_) ("funcky:types".List $_))'                                                                                          || BOOLEAN
        '"funcky:commons".equal ("funcky:types".List $_) "funcky:types".String'                                                                                                                   || FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".Record [])'                                                                                                              || FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".Record ["funcky:types".Type])'                                                                                           || FALSE
        '"funcky:commons".equal ("funcky:types".List $_) ("funcky:types".Record [$_])'                                                                                                            || FALSE
        '"funcky:commons".equal ("funcky:types".List $_) "funcky:types".Unit'                                                                                                                     || FALSE
        '"funcky:commons".equal ("funcky:types".List $_) $_'                                                                                                                                      || FALSE
        '"funcky:commons".equal "funcky:types".String "funcky:types".Type'                                                                                                                        || FALSE
        '"funcky:commons".equal "funcky:types".String "funcky:types".Number'                                                                                                                      || FALSE
        '"funcky:commons".equal "funcky:types".String "funcky:types".Boolean'                                                                                                                     || FALSE
        '"funcky:commons".equal "funcky:types".String "funcky:types".Character'                                                                                                                   || FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                        || FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".Function "funcky:types".Type $_)'                                                                                           || FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".Function $_ "funcky:types".Number)'                                                                                         || FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".Function $_ $_)'                                                                                                            || FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".List "funcky:types".Type)'                                                                                                  || FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".List $_)'                                                                                                                   || FALSE
        '"funcky:commons".equal "funcky:types".String "funcky:types".String'                                                                                                                      || TRUE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".Record [])'                                                                                                                 || FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".Record ["funcky:types".Type])'                                                                                              || FALSE
        '"funcky:commons".equal "funcky:types".String ("funcky:types".Record [$_])'                                                                                                               || FALSE
        '"funcky:commons".equal "funcky:types".String "funcky:types".Unit'                                                                                                                        || FALSE
        '"funcky:commons".equal "funcky:types".String $_'                                                                                                                                         || FALSE
        '"funcky:commons".equal ("funcky:types".Record []) "funcky:types".Type'                                                                                                                   || FALSE
        '"funcky:commons".equal ("funcky:types".Record []) "funcky:types".Number'                                                                                                                 || FALSE
        '"funcky:commons".equal ("funcky:types".Record []) "funcky:types".Boolean'                                                                                                                || FALSE
        '"funcky:commons".equal ("funcky:types".Record []) "funcky:types".Character'                                                                                                              || FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                   || FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".Function "funcky:types".Type $_)'                                                                                      || FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".Function $_ "funcky:types".Number)'                                                                                    || FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".Function $_ $_)'                                                                                                       || FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".List "funcky:types".Type)'                                                                                             || FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".List $_)'                                                                                                              || FALSE
        '"funcky:commons".equal ("funcky:types".Record []) "funcky:types".String'                                                                                                                 || FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".Record [])'                                                                                                            || TRUE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".Record ["funcky:types".Type])'                                                                                         || FALSE
        '"funcky:commons".equal ("funcky:types".Record []) ("funcky:types".Record [$_])'                                                                                                          || FALSE
        '"funcky:commons".equal ("funcky:types".Record []) "funcky:types".Unit'                                                                                                                   || TRUE
        '"funcky:commons".equal ("funcky:types".Record []) $_'                                                                                                                                    || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Type'                                                                                                || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Number'                                                                                              || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Boolean'                                                                                             || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Character'                                                                                           || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function "funcky:types".Type $_)'                                                                   || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function $_ "funcky:types".Number)'                                                                 || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function $_ $_)'                                                                                    || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".List "funcky:types".Type)'                                                                          || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".List $_)'                                                                                           || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) "funcky:types".String'                                                                                              || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record [])'                                                                                         || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record ["funcky:types".Type])'                                                                      || TRUE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record ["funcky:types".Number])'                                                                    || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record [$_])'                                                                                       || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Unit'                                                                                                || FALSE
        '"funcky:commons".equal ("funcky:types".Record ["funcky:types".Type]) $_'                                                                                                                 || FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) "funcky:types".Type'                                                                                                                 || FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) "funcky:types".Number'                                                                                                               || FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) "funcky:types".Boolean'                                                                                                              || FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) "funcky:types".Character'                                                                                                            || FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                 || FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".Function "funcky:types".Type $_)'                                                                                    || FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".Function $_ "funcky:types".Number)'                                                                                  || FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".Function $_ $_)'                                                                                                     || FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".List "funcky:types".Type)'                                                                                           || FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".List $_)'                                                                                                            || FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) "funcky:types".String'                                                                                                               || FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".Record [])'                                                                                                          || FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".Record ["funcky:types".Type])'                                                                                       || FALSE
        '"funcky:types".type ("funcky:commons".equal ("funcky:types".Record [$_]) ("funcky:types".Record [$_]))'                                                                                  || BOOLEAN
        '"funcky:commons".equal ("funcky:types".Record [$_]) "funcky:types".Unit'                                                                                                                 || FALSE
        '"funcky:commons".equal ("funcky:types".Record [$_]) $_'                                                                                                                                  || FALSE
        '"funcky:commons".equal "funcky:types".Unit "funcky:types".Type'                                                                                                                          || FALSE
        '"funcky:commons".equal "funcky:types".Unit "funcky:types".Number'                                                                                                                        || FALSE
        '"funcky:commons".equal "funcky:types".Unit "funcky:types".Boolean'                                                                                                                       || FALSE
        '"funcky:commons".equal "funcky:types".Unit "funcky:types".Character'                                                                                                                     || FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                          || FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".Function "funcky:types".Type $_)'                                                                                             || FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".Function $_ "funcky:types".Number)'                                                                                           || FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".Function $_ $_)'                                                                                                              || FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".List "funcky:types".Type)'                                                                                                    || FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".List $_)'                                                                                                                     || FALSE
        '"funcky:commons".equal "funcky:types".Unit "funcky:types".String'                                                                                                                        || FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".Record [])'                                                                                                                   || TRUE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".Record ["funcky:types".Type])'                                                                                                || FALSE
        '"funcky:commons".equal "funcky:types".Unit ("funcky:types".Record [$_])'                                                                                                                 || FALSE
        '"funcky:commons".equal "funcky:types".Unit "funcky:types".Unit'                                                                                                                          || TRUE
        '"funcky:commons".equal "funcky:types".Unit $_'                                                                                                                                           || FALSE
        '"funcky:commons".equal $_ "funcky:types".Type'                                                                                                                                           || FALSE
        '"funcky:commons".equal $_ "funcky:types".Number'                                                                                                                                         || FALSE
        '"funcky:commons".equal $_ "funcky:types".Boolean'                                                                                                                                        || FALSE
        '"funcky:commons".equal $_ "funcky:types".Character'                                                                                                                                      || FALSE
        '"funcky:commons".equal $_ ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                                           || FALSE
        '"funcky:commons".equal $_ ("funcky:types".Function "funcky:types".Type $_)'                                                                                                              || FALSE
        '"funcky:commons".equal $_ ("funcky:types".Function $_ "funcky:types".Number)'                                                                                                            || FALSE
        '"funcky:commons".equal $_ ("funcky:types".Function $_ $_)'                                                                                                                               || FALSE
        '"funcky:commons".equal $_ ("funcky:types".List "funcky:types".Type)'                                                                                                                     || FALSE
        '"funcky:commons".equal $_ ("funcky:types".List $_)'                                                                                                                                      || FALSE
        '"funcky:commons".equal $_ "funcky:types".String'                                                                                                                                         || FALSE
        '"funcky:commons".equal $_ ("funcky:types".Record [])'                                                                                                                                    || FALSE
        '"funcky:commons".equal $_ ("funcky:types".Record ["funcky:types".Type])'                                                                                                                 || FALSE
        '"funcky:commons".equal $_ ("funcky:types".Record [$_])'                                                                                                                                  || FALSE
        '"funcky:commons".equal $_ "funcky:types".Unit'                                                                                                                                           || FALSE
        '"funcky:types".type ("funcky:commons".equal $_ $_)'                                                                                                                                      || BOOLEAN
        '"funcky:commons".equal 0 0'                                                                                                                                                              || TRUE
        '"funcky:commons".equal 0 1'                                                                                                                                                              || FALSE
        '"funcky:commons".equal 1 0'                                                                                                                                                              || FALSE
        '"funcky:commons".equal 1 1'                                                                                                                                                              || TRUE
        '"funcky:commons".equal "funcky:booleans".false "funcky:booleans".false'                                                                                                                  || TRUE
        '"funcky:commons".equal "funcky:booleans".false "funcky:booleans".true'                                                                                                                   || FALSE
        '"funcky:commons".equal "funcky:booleans".true "funcky:booleans".false'                                                                                                                   || FALSE
        '"funcky:commons".equal "funcky:booleans".true "funcky:booleans".true'                                                                                                                    || TRUE
        '"funcky:commons".equal \'a\' \'a\''                                                                                                                                                      || TRUE
        '"funcky:commons".equal \'a\' \'b\''                                                                                                                                                      || FALSE
        '"funcky:commons".equal \'b\' \'a\''                                                                                                                                                      || FALSE
        '"funcky:commons".equal \'b\' \'b\''                                                                                                                                                      || TRUE
        '"funcky:commons".equal "funcky:numbers".add "funcky:numbers".add'                                                                                                                        || TRUE
        '"funcky:commons".equal "funcky:numbers".add "funcky:numbers".subtract'                                                                                                                   || FALSE
        '"funcky:commons".equal "funcky:numbers".subtract "funcky:numbers".add'                                                                                                                   || FALSE
        '"funcky:commons".equal "funcky:numbers".subtract "funcky:numbers".subtract'                                                                                                              || TRUE
        '"funcky:commons".equal ("funcky:numbers".add 0) ("funcky:numbers".add 0)'                                                                                                                || TRUE
        '"funcky:commons".equal ("funcky:numbers".add 0) ("funcky:numbers".add 1)'                                                                                                                || FALSE
        '"funcky:commons".equal ("funcky:numbers".add 1) ("funcky:numbers".add 0)'                                                                                                                || FALSE
        '"funcky:commons".equal ("funcky:numbers".add 1) ("funcky:numbers".add 1)'                                                                                                                || TRUE
        '"funcky:commons".equal [] []'                                                                                                                                                            || TRUE
        '"funcky:commons".equal [] [0]'                                                                                                                                                           || FALSE
        '"funcky:commons".equal [] [0, 1]'                                                                                                                                                        || FALSE
        '"funcky:commons".equal [0] []'                                                                                                                                                           || FALSE
        '"funcky:commons".equal [0] [0]'                                                                                                                                                          || TRUE
        '"funcky:commons".equal [0] [0, 1]'                                                                                                                                                       || FALSE
        '"funcky:commons".equal [0, 1] []'                                                                                                                                                        || FALSE
        '"funcky:commons".equal [0, 1] [0]'                                                                                                                                                       || FALSE
        '"funcky:commons".equal [0, 1] [0, 1]'                                                                                                                                                    || TRUE
        '"funcky:commons".equal "" ""'                                                                                                                                                            || TRUE
        '"funcky:commons".equal "" "a"'                                                                                                                                                           || FALSE
        '"funcky:commons".equal "" "b"'                                                                                                                                                           || FALSE
        '"funcky:commons".equal "a" ""'                                                                                                                                                           || FALSE
        '"funcky:commons".equal "a" "a"'                                                                                                                                                          || TRUE
        '"funcky:commons".equal "a" "b"'                                                                                                                                                          || FALSE
        '"funcky:commons".equal "b" ""'                                                                                                                                                           || FALSE
        '"funcky:commons".equal "b" "a"'                                                                                                                                                          || FALSE
        '"funcky:commons".equal "b" "b"'                                                                                                                                                          || TRUE
        '"funcky:commons".equal {} {}'                                                                                                                                                            || TRUE
        '"funcky:commons".equal {0} {0}'                                                                                                                                                          || TRUE
        '"funcky:commons".equal {0} {1}'                                                                                                                                                          || FALSE
        '"funcky:commons".equal {1} {0}'                                                                                                                                                          || FALSE
        '"funcky:commons".equal {1} {1}'                                                                                                                                                          || TRUE
        '"funcky:commons".equal {0, \'a\'} {0, \'a\'}'                                                                                                                                            || TRUE
        '"funcky:commons".equal {0, \'a\'} {0, \'b\'}'                                                                                                                                            || FALSE
        '"funcky:commons".equal {0, \'a\'} {1, \'a\'}'                                                                                                                                            || FALSE
        '"funcky:commons".equal {0, \'a\'} {1, \'b\'}'                                                                                                                                            || FALSE
        '"funcky:commons".equal {0, \'b\'} {0, \'a\'}'                                                                                                                                            || FALSE
        '"funcky:commons".equal {0, \'b\'} {0, \'b\'}'                                                                                                                                            || TRUE
        '"funcky:commons".equal {0, \'b\'} {1, \'a\'}'                                                                                                                                            || FALSE
        '"funcky:commons".equal {0, \'b\'} {1, \'b\'}'                                                                                                                                            || FALSE
        '"funcky:commons".equal {1, \'a\'} {0, \'a\'}'                                                                                                                                            || FALSE
        '"funcky:commons".equal {1, \'a\'} {0, \'b\'}'                                                                                                                                            || FALSE
        '"funcky:commons".equal {1, \'a\'} {1, \'a\'}'                                                                                                                                            || TRUE
        '"funcky:commons".equal {1, \'a\'} {1, \'b\'}'                                                                                                                                            || FALSE
        '"funcky:commons".equal {1, \'b\'} {0, \'a\'}'                                                                                                                                            || FALSE
        '"funcky:commons".equal {1, \'b\'} {0, \'b\'}'                                                                                                                                            || FALSE
        '"funcky:commons".equal {1, \'b\'} {1, \'a\'}'                                                                                                                                            || FALSE
        '"funcky:commons".equal {1, \'b\'} {1, \'b\'}'                                                                                                                                            || TRUE
    }

    @Unroll('Test greaterThan (expression: #expression)')
    def 'Test greaterThan'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                            || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:commons".greaterThan)'                                                                                                                         || FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".greaterThan))'                                                                                              || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".greaterThan)))'                                                                       || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:commons".greaterThan)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".greaterThan)))' || TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".greaterThan))'                                                                                                      || BOOLEAN
        '"funcky:types".type ("funcky:commons".greaterThan 0)'                                                                                                                                                || FUNCTION(NUMBER, BOOLEAN)
        '"funcky:types".type ("funcky:commons".greaterThan \'a\')'                                                                                                                                            || FUNCTION(CHARACTER, BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:commons".greaterThan ("funcky:commons".error "foo")))'                                                                                        || FALSE
        '"funcky:commons".greaterThan 0 0'                                                                                                                                                                    || FALSE
        '"funcky:commons".greaterThan 0 1'                                                                                                                                                                    || FALSE
        '"funcky:commons".greaterThan 1 0'                                                                                                                                                                    || TRUE
        '"funcky:commons".greaterThan 1 1'                                                                                                                                                                    || FALSE
        '"funcky:commons".greaterThan \'a\' \'a\''                                                                                                                                                            || FALSE
        '"funcky:commons".greaterThan \'a\' \'b\''                                                                                                                                                            || FALSE
        '"funcky:commons".greaterThan \'b\' \'a\''                                                                                                                                                            || TRUE
        '"funcky:commons".greaterThan \'b\' \'b\''                                                                                                                                                            || FALSE
    }

    @Unroll('Test lessThan (expression: #expression)')
    def 'Test lessThan'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                      || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:commons".lessThan)'                                                                                                                      || FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".lessThan))'                                                                                           || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".lessThan)))'                                                                    || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:commons".lessThan)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".lessThan)))' || TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".lessThan))'                                                                                                   || BOOLEAN
        '"funcky:types".type ("funcky:commons".lessThan 0)'                                                                                                                                             || FUNCTION(NUMBER, BOOLEAN)
        '"funcky:types".type ("funcky:commons".lessThan \'a\')'                                                                                                                                         || FUNCTION(CHARACTER, BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:commons".lessThan ("funcky:commons".error "foo")))'                                                                                     || FALSE
        '"funcky:commons".lessThan 0 0'                                                                                                                                                                 || FALSE
        '"funcky:commons".lessThan 0 1'                                                                                                                                                                 || TRUE
        '"funcky:commons".lessThan 1 0'                                                                                                                                                                 || FALSE
        '"funcky:commons".lessThan 1 1'                                                                                                                                                                 || FALSE
        '"funcky:commons".lessThan \'a\' \'a\''                                                                                                                                                         || FALSE
        '"funcky:commons".lessThan \'a\' \'b\''                                                                                                                                                         || TRUE
        '"funcky:commons".lessThan \'b\' \'a\''                                                                                                                                                         || FALSE
        '"funcky:commons".lessThan \'b\' \'b\''                                                                                                                                                         || FALSE
    }

    @Unroll('Test greaterEqual (expression: #expression)')
    def 'Test greaterEqual'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                              || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:commons".greaterEqual)'                                                                                                                          || FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".greaterEqual))'                                                                                               || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".greaterEqual)))'                                                                        || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:commons".greaterEqual)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".greaterEqual)))' || TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".greaterEqual))'                                                                                                       || BOOLEAN
        '"funcky:types".type ("funcky:commons".greaterEqual 0)'                                                                                                                                                 || FUNCTION(NUMBER, BOOLEAN)
        '"funcky:types".type ("funcky:commons".greaterEqual \'a\')'                                                                                                                                             || FUNCTION(CHARACTER, BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:commons".greaterEqual ("funcky:commons".error "foo")))'                                                                                         || FALSE
        '"funcky:commons".greaterEqual 0 0'                                                                                                                                                                     || TRUE
        '"funcky:commons".greaterEqual 0 1'                                                                                                                                                                     || FALSE
        '"funcky:commons".greaterEqual 1 0'                                                                                                                                                                     || TRUE
        '"funcky:commons".greaterEqual 1 1'                                                                                                                                                                     || TRUE
        '"funcky:commons".greaterEqual \'a\' \'a\''                                                                                                                                                             || TRUE
        '"funcky:commons".greaterEqual \'a\' \'b\''                                                                                                                                                             || FALSE
        '"funcky:commons".greaterEqual \'b\' \'a\''                                                                                                                                                             || TRUE
        '"funcky:commons".greaterEqual \'b\' \'b\''                                                                                                                                                             || TRUE
    }

    @Unroll('Test lessEqual (expression: #expression)')
    def 'Test lessEqual'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                        || result
        '"funcky:lists".empty ("funcky:commons".string "funcky:commons".lessEqual)'                                                                                                                       || FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".lessEqual))'                                                                                            || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".lessEqual)))'                                                                     || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:commons".lessEqual)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".lessEqual)))' || TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".lessEqual))'                                                                                                    || BOOLEAN
        '"funcky:types".type ("funcky:commons".lessEqual 0)'                                                                                                                                              || FUNCTION(NUMBER, BOOLEAN)
        '"funcky:types".type ("funcky:commons".lessEqual \'a\')'                                                                                                                                          || FUNCTION(CHARACTER, BOOLEAN)
        '"funcky:lists".empty ("funcky:commons".string ("funcky:commons".lessEqual ("funcky:commons".error "foo")))'                                                                                      || FALSE
        '"funcky:commons".lessEqual 0 0'                                                                                                                                                                  || TRUE
        '"funcky:commons".lessEqual 0 1'                                                                                                                                                                  || TRUE
        '"funcky:commons".lessEqual 1 0'                                                                                                                                                                  || FALSE
        '"funcky:commons".lessEqual 1 1'                                                                                                                                                                  || TRUE
        '"funcky:commons".lessEqual \'a\' \'a\''                                                                                                                                                          || TRUE
        '"funcky:commons".lessEqual \'a\' \'b\''                                                                                                                                                          || TRUE
        '"funcky:commons".lessEqual \'b\' \'a\''                                                                                                                                                          || FALSE
        '"funcky:commons".lessEqual \'b\' \'b\''                                                                                                                                                          || TRUE
    }

    @Unroll('Test compare (expression: #expression)')
    def 'Test compare'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                    || result
        '"funcky:commons".compare'                                                                                                                                                                    || new Commons().$compare
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".compare))'                                                                                          || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".compare)))'                                                                   || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:commons".compare)) ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".compare)))' || TRUE
        '"funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".compare))'                                                                                                  || NUMBER
        '"funcky:types".type ("funcky:commons".compare "funcky:types".Type)'                                                                                                                          || FUNCTION(TYPE, NUMBER)
        '"funcky:types".type ("funcky:commons".compare 0)'                                                                                                                                            || FUNCTION(NUMBER, NUMBER)
        '"funcky:commons".string ("funcky:commons".compare ("funcky:commons".error "foo"))'                                                                                                           || new FunckyJavaConverter().convert('"funcky:commons".compare ("funcky:commons".error "foo")')
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
        '"funcky:types".type ("funcky:commons".compare ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type $_))'                                            || NUMBER
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
        '"funcky:types".type ("funcky:commons".compare ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number))'                                        || NUMBER
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
        '"funcky:types".type ("funcky:commons".compare ("funcky:types".Function $_ $_) ("funcky:types".Function $_ "funcky:types".Number))'                                                           || NUMBER
        '"funcky:types".type ("funcky:commons".compare ("funcky:types".Function $_ $_) ("funcky:types".Function $_ $_))'                                                                              || NUMBER
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
        '"funcky:types".type ("funcky:commons".compare ("funcky:types".List $_) ("funcky:types".List $_))'                                                                                            || NUMBER
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
        '"funcky:types".type ("funcky:commons".compare ("funcky:types".Record [$_]) ("funcky:types".Record [$_]))'                                                                                    || NUMBER
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
        '"funcky:types".type ("funcky:commons".compare $_ $_)'                                                                                                                                        || NUMBER
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
        '"funcky:commons".hash'                                                                                                                                                                                          || new Commons().$hash
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".hash))'                                                                                                                || TRUE
        '"funcky:types".range ("funcky:types".type "funcky:commons".hash)'                                                                                                                                               || NUMBER
        '"funcky:commons".equal ("funcky:commons".hash "funcky:types".Type) ("funcky:commons".hash "funcky:types".Type)'                                                                                                 || TRUE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:types".Number) ("funcky:commons".hash "funcky:types".Number)'                                                                                             || TRUE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:types".Boolean) ("funcky:commons".hash "funcky:types".Boolean)'                                                                                           || TRUE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:types".Character) ("funcky:commons".hash "funcky:types".Character)'                                                                                       || TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".Function "funcky:types".Type "funcky:types".Number)) ("funcky:commons".hash ("funcky:types".Function "funcky:types".Type "funcky:types".Number))' || TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".Function "funcky:types".Type $_)) ("funcky:commons".hash ("funcky:types".Function "funcky:types".Type $_))'                                       || FALSE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".Function $_ "funcky:types".Number)) ("funcky:commons".hash ("funcky:types".Function $_ "funcky:types".Number))'                                   || FALSE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".Function $_ $_)) ("funcky:commons".hash ("funcky:types".Function $_ $_))'                                                                         || FALSE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".List "funcky:types".Type)) ("funcky:commons".hash ("funcky:types".List "funcky:types".Type))'                                                     || TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".List $_)) ("funcky:commons".hash ("funcky:types".List $_))'                                                                                       || FALSE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:types".String) ("funcky:commons".hash "funcky:types".String)'                                                                                             || TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".Record [])) ("funcky:commons".hash ("funcky:types".Record []))'                                                                                   || TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".Record ["funcky:types".Type])) ("funcky:commons".hash ("funcky:types".Record ["funcky:types".Type]))'                                             || TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:types".Record [$_])) ("funcky:commons".hash ("funcky:types".Record [$_]))'                                                                               || FALSE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:types".Unit) ("funcky:commons".hash "funcky:types".Unit)'                                                                                                 || TRUE
        '"funcky:commons".equal ("funcky:commons".hash $_) ("funcky:commons".hash $_)'                                                                                                                                   || FALSE
        '"funcky:commons".equal ("funcky:commons".hash 0) ("funcky:commons".hash 0)'                                                                                                                                     || TRUE
        '"funcky:commons".equal ("funcky:commons".hash 1) ("funcky:commons".hash 1)'                                                                                                                                     || TRUE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:booleans".false) ("funcky:commons".hash "funcky:booleans".false)'                                                                                         || TRUE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:booleans".true) ("funcky:commons".hash "funcky:booleans".true)'                                                                                           || TRUE
        '"funcky:commons".equal ("funcky:commons".hash \'a\') ("funcky:commons".hash \'a\')'                                                                                                                             || TRUE
        '"funcky:commons".equal ("funcky:commons".hash \'b\') ("funcky:commons".hash \'b\')'                                                                                                                             || TRUE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:numbers".add) ("funcky:commons".hash "funcky:numbers".add)'                                                                                               || TRUE
        '"funcky:commons".equal ("funcky:commons".hash "funcky:booleans".and) ("funcky:commons".hash "funcky:booleans".and)'                                                                                             || TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:numbers".add 0)) ("funcky:commons".hash ("funcky:numbers".add 0))'                                                                                       || TRUE
        '"funcky:commons".equal ("funcky:commons".hash ("funcky:booleans".and "funcky:booleans".false)) ("funcky:commons".hash ("funcky:booleans".and "funcky:booleans".false))'                                         || TRUE
        '"funcky:commons".equal ("funcky:commons".hash []) ("funcky:commons".hash [])'                                                                                                                                   || TRUE
        '"funcky:commons".equal ("funcky:commons".hash [0]) ("funcky:commons".hash [0])'                                                                                                                                 || TRUE
        '"funcky:commons".equal ("funcky:commons".hash [0, 1]) ("funcky:commons".hash [0, 1])'                                                                                                                           || TRUE
        '"funcky:commons".equal ("funcky:commons".hash "") ("funcky:commons".hash "")'                                                                                                                                   || TRUE
        '"funcky:commons".equal ("funcky:commons".hash "foo") ("funcky:commons".hash "foo")'                                                                                                                             || TRUE
        '"funcky:commons".equal ("funcky:commons".hash {}) ("funcky:commons".hash {})'                                                                                                                                   || TRUE
        '"funcky:commons".equal ("funcky:commons".hash {0}) ("funcky:commons".hash {0})'                                                                                                                                 || TRUE
        '"funcky:commons".equal ("funcky:commons".hash {0, \'a\'}) ("funcky:commons".hash {0, \'a\'})'                                                                                                                   || TRUE
    }

    @Unroll('Test if (expression: #expression)')
    def 'Test if'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                                                        || result
        '"funcky:commons".if'                                                                                                                                                                                                             || new Commons().$if
        '"funcky:types".domain ("funcky:types".type "funcky:commons".if)'                                                                                                                                                                 || BOOLEAN
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".if)))'                                                                                                            || TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".if))))'                                                                                     || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".if))))'                                                                                      || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".if))) ("funcky:types".domain ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".if))))' || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".range ("funcky:types".type "funcky:commons".if))) ("funcky:types".range ("funcky:types".range ("funcky:types".range ("funcky:types".type "funcky:commons".if))))'  || TRUE
        '"funcky:commons".string ("funcky:commons".if ("funcky:commons".error "foo"))'                                                                                                                                                    || new FunckyJavaConverter().convert('"funcky:commons".if ("funcky:commons".error "foo")')
        '"funcky:types".type ("funcky:commons".if "funcky:booleans".false 1)'                                                                                                                                                             || FUNCTION(NUMBER, NUMBER)
        '"funcky:types".type ("funcky:commons".if "funcky:booleans".false \'a\')'                                                                                                                                                         || FUNCTION(CHARACTER, CHARACTER)
        '"funcky:commons".string ("funcky:commons".if "funcky:booleans".false ("funcky:commons".error "foo"))'                                                                                                                            || new FunckyJavaConverter().convert('"funcky:commons".if "funcky:booleans".false ("funcky:commons".error "foo")')
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
        '"funcky:commons".string'                                                                           || new Commons().$string
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".string))' || TRUE
        '"funcky:types".range ("funcky:types".type "funcky:commons".string)'                                || STRING
        '"funcky:commons".string "funcky:types".Type'                                                       || new FunckyJavaConverter().convert('"funcky:types".Type')
        '"funcky:commons".string "funcky:types".Number'                                                     || new FunckyJavaConverter().convert('"funcky:types".Number')
        '"funcky:commons".string "funcky:types".Boolean'                                                    || new FunckyJavaConverter().convert('"funcky:types".Boolean')
        '"funcky:commons".string "funcky:types".Character'                                                  || new FunckyJavaConverter().convert('"funcky:types".Character')
        '"funcky:commons".string ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'       || new FunckyJavaConverter().convert('"funcky:types".Function "funcky:types".Type "funcky:types".Number')
        '"funcky:commons".string ("funcky:types".List "funcky:types".Type)'                                 || new FunckyJavaConverter().convert('"funcky:types".List "funcky:types".Type')
        '"funcky:commons".string "funcky:types".String'                                                     || new FunckyJavaConverter().convert('"funcky:types".List "funcky:types".Character')
        '"funcky:commons".string ("funcky:types".Record [])'                                                || new FunckyJavaConverter().convert('"funcky:types".Record []')
        '"funcky:commons".string ("funcky:types".Record ["funcky:types".Type])'                             || new FunckyJavaConverter().convert('"funcky:types".Record ["funcky:types".Type]')
        '"funcky:commons".string "funcky:types".Unit'                                                       || new FunckyJavaConverter().convert('"funcky:types".Record []')
        '"funcky:lists".empty ("funcky:commons".string $_)'                                                 || FALSE
        '"funcky:commons".string 0'                                                                         || new FunckyJavaConverter().convert('0')
        '"funcky:commons".string 1'                                                                         || new FunckyJavaConverter().convert('1')
        '"funcky:commons".string "funcky:booleans".false'                                                   || new FunckyJavaConverter().convert('"funcky:booleans".false')
        '"funcky:commons".string "funcky:booleans".true'                                                    || new FunckyJavaConverter().convert('"funcky:booleans".true')
        '"funcky:commons".string \'a\''                                                                     || new FunckyJavaConverter().convert('a')
        '"funcky:commons".string \'b\''                                                                     || new FunckyJavaConverter().convert('b')
        '"funcky:commons".string "funcky:types".type'                                                       || new FunckyJavaConverter().convert('"funcky:types".type')
        '"funcky:commons".string ("funcky:numbers".add 0)'                                                  || new FunckyJavaConverter().convert('"funcky:numbers".add 0')
        '"funcky:commons".string ("funcky:numbers".add ("funcky:commons".error "foo"))'                     || new FunckyJavaConverter().convert('"funcky:numbers".add ("funcky:commons".error "foo")')
        '"funcky:commons".string []'                                                                        || new FunckyJavaConverter().convert('[]')
        '"funcky:commons".string [0]'                                                                       || new FunckyJavaConverter().convert('[0]')
        '"funcky:commons".string [0, 1]'                                                                    || new FunckyJavaConverter().convert('[0, 1]')
        '"funcky:commons".string ["funcky:numbers".add 1 2]'                                                || new FunckyJavaConverter().convert('[3]')
        '"funcky:commons".string ""'                                                                        || new FunckyJavaConverter().convert('')
        '"funcky:commons".string "foo"'                                                                     || new FunckyJavaConverter().convert('foo')
        '"funcky:commons".string {}'                                                                        || new FunckyJavaConverter().convert('{}')
        '"funcky:commons".string {0}'                                                                       || new FunckyJavaConverter().convert('{0}')
        '"funcky:commons".string {0, \'a\'}'                                                                || new FunckyJavaConverter().convert('{0, a}')
    }

    @Unroll('Test number (expression: #expression)')
    def 'Test number'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                    || result
        '"funcky:commons".number'                     || new Commons().$number
        '"funcky:types".type "funcky:commons".number' || FUNCTION(STRING, NUMBER)
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
        '"funcky:commons".error'                                                                          || new Commons().$error
        '"funcky:types".domain ("funcky:types".type "funcky:commons".error)'                              || STRING
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".type "funcky:commons".error))' || TRUE
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
        '"funcky:commons".bottom'                                                                                                                                           || new Commons().$bottom
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:commons".bottom))'                                                                 || TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".type "funcky:commons".bottom))'                                                                  || TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".type "funcky:commons".bottom)) ("funcky:types".range ("funcky:types".type "funcky:commons".bottom))' || FALSE
    }
}
