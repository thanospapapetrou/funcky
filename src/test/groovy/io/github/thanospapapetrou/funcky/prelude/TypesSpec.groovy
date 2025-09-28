package io.github.thanospapapetrou.funcky.prelude

import java.util.regex.Pattern

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException
import io.github.thanospapapetrou.funcky.runtime.prelude.Types
import spock.lang.Unroll

import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.FALSE
import static io.github.thanospapapetrou.funcky.runtime.FunckyBoolean.TRUE
import static io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType.FUNCTION
import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.LIST
import static io.github.thanospapapetrou.funcky.runtime.FunckyListType.STRING
import static io.github.thanospapapetrou.funcky.runtime.FunckyRecordType.RECORD
import static io.github.thanospapapetrou.funcky.runtime.FunckyRecordType.UNIT
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.BOOLEAN
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.CHARACTER
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.NUMBER
import static io.github.thanospapapetrou.funcky.runtime.FunckySimpleType.TYPE

class TypesSpec extends BaseSpec {
    @Unroll('Test simple types (expression: #expression)')
    def 'Test simple types'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                     || result
        '"funcky:types".Type'                          || TYPE.apply(engine)
        '"funcky:types".type "funcky:types".Type'      || TYPE.apply(engine)
        '"funcky:types".Number'                        || NUMBER.apply(engine)
        '"funcky:types".type "funcky:types".Number'    || TYPE.apply(engine)
        '"funcky:types".Boolean'                       || BOOLEAN.apply(engine)
        '"funcky:types".type "funcky:types".Boolean'   || TYPE.apply(engine)
        '"funcky:types".Character'                     || CHARACTER.apply(engine)
        '"funcky:types".type "funcky:types".Character' || TYPE.apply(engine)
    }

    @Unroll('Test Function (expression: #expression)')
    def 'Test Function'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                             || result
        '"funcky:types".Function'                                                                              || new Types(engine).$Function
        '"funcky:types".type "funcky:types".Function'                                                          || FUNCTION(TYPE, TYPE, TYPE).apply(engine)
        '"funcky:types".type ("funcky:types".Function "funcky:types".Type)'                                    || FUNCTION(TYPE, TYPE).apply(engine)
        '"funcky:commons".string ("funcky:types".Function ("funcky:commons".error "foo"))'                     || engine.converter.convert('"funcky:types".Function ("funcky:commons".error "foo")')
        '"funcky:types".Function "funcky:types".Type "funcky:types".Number'                                    || FUNCTION(TYPE, NUMBER).apply(engine)
        '"funcky:commons".string ("funcky:types".Function "funcky:types".Type ("funcky:commons".error "foo"))' || engine.converter.convert('"funcky:types".Function "funcky:types".Type ("funcky:commons".error "foo")')
    }

    @Unroll('Test domain (expression: #expression)')
    def 'Test domain'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                  || result
        '"funcky:types".domain'                                                                     || new Types(engine).$domain
        '"funcky:types".type "funcky:types".domain'                                                 || FUNCTION(TYPE, TYPE).apply(engine)
        '"funcky:types".domain ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || TYPE.apply(engine)
    }

    def 'Test domain (runtime error)'() {
        when:
        engine.eval('"funcky:types".domain "funcky:types".Type')
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(String.format(Types.ERROR_DOMAIN, TYPE.apply(engine)))
    }

    @Unroll('Test range (expression: #expression)')
    def 'Test range'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                 || result
        '"funcky:types".range'                                                                     || new Types(engine).$range
        '"funcky:types".type "funcky:types".range'                                                 || FUNCTION(TYPE, TYPE).apply(engine)
        '"funcky:types".range ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || NUMBER.apply(engine)
    }

    def 'Test range (runtime error)'() {
        when:
        engine.eval('"funcky:types".range "funcky:types".Type')
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(String.format(Types.ERROR_RANGE, TYPE.apply(engine)))
    }

    @Unroll('Test List (expression: #expression)')
    def 'Test List'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                     || result
        '"funcky:types".List'                                                          || new Types(engine).$List
        '"funcky:types".type "funcky:types".List'                                      || FUNCTION(TYPE, TYPE).apply(engine)
        '"funcky:types".List "funcky:types".Type'                                      || LIST(TYPE).apply(engine)
        '"funcky:commons".string ("funcky:types".List ("funcky:commons".error "foo"))' || engine.converter.convert('"funcky:types".List ("funcky:commons".error "foo")')
    }

    @Unroll('Test element (expression: #expression)')
    def 'Test element'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                         || result
        '"funcky:types".element'                                           || new Types(engine).$element
        '"funcky:types".type "funcky:types".element'                       || FUNCTION(TYPE, TYPE).apply(engine)
        '"funcky:types".element ("funcky:types".List "funcky:types".Type)' || TYPE.apply(engine)
    }

    def 'Test element (runtime error)'() {
        when:
        engine.eval('"funcky:types".element "funcky:types".Type')
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(String.format(Types.ERROR_ELEMENT, TYPE.apply(engine)))
    }

    @Unroll('Test String (expression: #expression)')
    def 'Test String'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                  || result
        '"funcky:types".String'                     || STRING.apply(engine)
        '"funcky:types".type "funcky:types".String' || TYPE.apply(engine)

    }

    @Unroll('Test Record (expression: #expression)')
    def 'Test Record'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                    || result
        '"funcky:types".Record'                       || new Types(engine).$Record
        '"funcky:types".type "funcky:types".Record'   || FUNCTION(LIST(TYPE), TYPE).apply(engine)
        '"funcky:types".Record []'                    || UNIT.apply(engine)
        '"funcky:types".Record ["funcky:types".Type]' || RECORD(TYPE).apply(engine)
    }

    @Unroll('Test components (expression: #expression)')
    def 'Test components'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                || result
        '"funcky:types".components'                                               || new Types(engine).$components
        '"funcky:types".type "funcky:types".components'                           || FUNCTION(TYPE, LIST(TYPE)).apply(engine)
        '"funcky:types".components ("funcky:types".Record [])'                    || engine.converter.convert([])
        '"funcky:types".components ("funcky:types".Record ["funcky:types".Type])' || engine.converter.convert([TYPE.apply(engine)])
    }

    def 'Test components (runtime error)'() {
        when:
        engine.eval('"funcky:types".components "funcky:types".Type')
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(String.format(Types.ERROR_COMPONENTS, TYPE.apply(engine)))
    }

    @Unroll('Test Unit (expression: #expression)')
    def 'Test Unit'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                || result
        '"funcky:types".Unit'                     || UNIT.apply(engine)
        '"funcky:types".type "funcky:types".Unit' || TYPE.apply(engine)
    }

    @Unroll('Test type (expression: #expression)')
    def 'Test type'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                      || result
        '"funcky:types".type'                                                                           || new Types(engine).$type
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:types".type))' || TRUE.apply(engine)
        '"funcky:types".type "funcky:types".Type'                                                       || TYPE.apply(engine)
        '"funcky:types".type 1'                                                                         || NUMBER.apply(engine)
        '"funcky:types".type "funcky:booleans".false'                                                   || BOOLEAN.apply(engine)
        '"funcky:types".type \'a\''                                                                     || CHARACTER.apply(engine)
        '"funcky:types".type "funcky:types".free'                                                       || FUNCTION(TYPE, TYPE).apply(engine)
        '"funcky:types".listType ("funcky:types".type [])'                                              || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".type []))'                 || TRUE.apply(engine)
        '"funcky:types".type [1]'                                                                       || LIST(NUMBER).apply(engine)
        '"funcky:types".type ""'                                                                        || STRING.apply(engine)
        '"funcky:types".type {}'                                                                        || UNIT.apply(engine)
        '"funcky:types".type {1}'                                                                       || RECORD(NUMBER).apply(engine)
        '"funcky:types".type {1, \'a\'}'                                                                || RECORD(NUMBER, CHARACTER).apply(engine)
    }

    @Unroll('Test typeVariable (expression: #expression)')
    def 'Test typeVariable'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                        || result
        '"funcky:types".typeVariable'                                                                     || new Types(engine).$typeVariable
        '"funcky:types".type "funcky:types".typeVariable'                                                 || FUNCTION(TYPE, BOOLEAN).apply(engine)
        '"funcky:types".typeVariable "funcky:types".Type'                                                 || FALSE.apply(engine)
        '"funcky:types".typeVariable "funcky:types".Number'                                               || FALSE.apply(engine)
        '"funcky:types".typeVariable "funcky:types".Boolean'                                              || FALSE.apply(engine)
        '"funcky:types".typeVariable "funcky:types".Character'                                            || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".List "funcky:types".Type)'                           || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".String)'                                             || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".Record [])'                                          || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".Record ["funcky:types".Type])'                       || FALSE.apply(engine)
        '"funcky:types".typeVariable "funcky:types".Unit'                                                 || FALSE.apply(engine)
        '"funcky:types".typeVariable $_'                                                                  || TRUE.apply(engine)
    }

    @Unroll('Test functionType (expression: #expression)')
    def 'Test functionType'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                        || result
        '"funcky:types".functionType'                                                                     || new Types(engine).$functionType
        '"funcky:types".type "funcky:types".functionType'                                                 || FUNCTION(TYPE, BOOLEAN).apply(engine)
        '"funcky:types".functionType "funcky:types".Type'                                                 || FALSE.apply(engine)
        '"funcky:types".functionType "funcky:types".Number'                                               || FALSE.apply(engine)
        '"funcky:types".functionType "funcky:types".Boolean'                                              || FALSE.apply(engine)
        '"funcky:types".functionType "funcky:types".Character'                                            || FALSE.apply(engine)
        '"funcky:types".functionType ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || TRUE.apply(engine)
        '"funcky:types".functionType ("funcky:types".List "funcky:types".Type)'                           || FALSE.apply(engine)
        '"funcky:types".functionType ("funcky:types".String)'                                             || FALSE.apply(engine)
        '"funcky:types".functionType ("funcky:types".Record [])'                                          || FALSE.apply(engine)
        '"funcky:types".functionType ("funcky:types".Record ["funcky:types".Type])'                       || FALSE.apply(engine)
        '"funcky:types".functionType "funcky:types".Unit'                                                 || FALSE.apply(engine)
        '"funcky:types".functionType $_'                                                                  || FALSE.apply(engine)
    }

    @Unroll('Test listType (expression: #expression)')
    def 'Test listType'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                    || result
        '"funcky:types".listType'                                                                     || new Types(engine).$listType
        '"funcky:types".type "funcky:types".listType'                                                 || FUNCTION(TYPE, BOOLEAN).apply(engine)
        '"funcky:types".listType "funcky:types".Type'                                                 || FALSE.apply(engine)
        '"funcky:types".listType "funcky:types".Number'                                               || FALSE.apply(engine)
        '"funcky:types".listType "funcky:types".Boolean'                                              || FALSE.apply(engine)
        '"funcky:types".listType "funcky:types".Character'                                            || FALSE.apply(engine)
        '"funcky:types".listType ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || FALSE.apply(engine)
        '"funcky:types".listType ("funcky:types".List "funcky:types".Type)'                           || TRUE.apply(engine)
        '"funcky:types".listType ("funcky:types".String)'                                             || TRUE.apply(engine)
        '"funcky:types".listType ("funcky:types".Record [])'                                          || FALSE.apply(engine)
        '"funcky:types".listType ("funcky:types".Record ["funcky:types".Type])'                       || FALSE.apply(engine)
        '"funcky:types".listType "funcky:types".Unit'                                                 || FALSE.apply(engine)
        '"funcky:types".listType $_'                                                                  || FALSE.apply(engine)
    }

    @Unroll('Test recordType (expression: #expression)')
    def 'Test recordType'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                      || result
        '"funcky:types".recordType'                                                                     || new Types(engine).$recordType
        '"funcky:types".type "funcky:types".recordType'                                                 || FUNCTION(TYPE, BOOLEAN).apply(engine)
        '"funcky:types".recordType "funcky:types".Type'                                                 || FALSE.apply(engine)
        '"funcky:types".recordType "funcky:types".Number'                                               || FALSE.apply(engine)
        '"funcky:types".recordType "funcky:types".Boolean'                                              || FALSE.apply(engine)
        '"funcky:types".recordType "funcky:types".Character'                                            || FALSE.apply(engine)
        '"funcky:types".recordType ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || FALSE.apply(engine)
        '"funcky:types".recordType ("funcky:types".List "funcky:types".Type)'                           || FALSE.apply(engine)
        '"funcky:types".recordType ("funcky:types".String)'                                             || FALSE.apply(engine)
        '"funcky:types".recordType ("funcky:types".Record [])'                                          || TRUE.apply(engine)
        '"funcky:types".recordType ("funcky:types".Record ["funcky:types".Type])'                       || TRUE.apply(engine)
        '"funcky:types".recordType "funcky:types".Unit'                                                 || TRUE.apply(engine)
        '"funcky:types".recordType $_'                                                                  || FALSE.apply(engine)
    }

    @Unroll('Test free (expression: #expression')
    def 'Test free'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                       || result
        '"funcky:types".free'                                                                                                                                                                            || new Types(engine).$free
        '"funcky:types".type "funcky:types".free'                                                                                                                                                        || FUNCTION(TYPE, TYPE).apply(engine)
        '"funcky:types".free "funcky:types".Type'                                                                                                                                                        || TYPE.apply(engine)
        '"funcky:types".free "funcky:types".Number'                                                                                                                                                      || NUMBER.apply(engine)
        '"funcky:types".free "funcky:types".Boolean'                                                                                                                                                     || BOOLEAN.apply(engine)
        '"funcky:types".free "funcky:types".Character'                                                                                                                                                   || CHARACTER.apply(engine)
        '"funcky:types".free ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                                                        || FUNCTION(TYPE, NUMBER).apply(engine)
        '"funcky:types".domain ("funcky:types".free ("funcky:types".Function "funcky:types".Type $_))'                                                                                                   || TYPE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".free ("funcky:types".Function "funcky:types".Type $_)))'                                                                      || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".free ("funcky:types".Function "funcky:types".Type $_))) ("funcky:types".range ("funcky:types".Function "funcky:types".Type $_))'   || FALSE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".free ("funcky:types".Function $_ "funcky:types".Type)))'                                                                     || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".free ("funcky:types".Function $_ "funcky:types".Type))) ("funcky:types".domain ("funcky:types".Function $_ "funcky:types".Type))' || FALSE.apply(engine)
        '"funcky:types".range ("funcky:types".free ("funcky:types".Function $_ "funcky:types".Type))'                                                                                                    || TYPE.apply(engine)
        '"funcky:types".free ("funcky:types".List "funcky:types".Type)'                                                                                                                                  || LIST(TYPE).apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".free ("funcky:types".List $_)))'                                                                                            || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".free ("funcky:types".List $_))) ("funcky:types".element ("funcky:types".List $_))'                                               || FALSE.apply(engine)
        '"funcky:types".free "funcky:types".String'                                                                                                                                                      || STRING.apply(engine)
        '"funcky:types".free ("funcky:types".Record [])'                                                                                                                                                 || UNIT.apply(engine)
        '"funcky:types".free ("funcky:types".Record ["funcky:types".Type])'                                                                                                                              || RECORD(TYPE).apply(engine)
        '"funcky:types".typeVariable ("funcky:lists".head ("funcky:types".components ("funcky:types".free ("funcky:types".Record [$_]))))'                                                               || TRUE.apply(engine)
        '"funcky:lists".empty ("funcky:lists".tail ("funcky:types".components ("funcky:types".free ("funcky:types".Record [$_]))))'                                                                      || TRUE.apply(engine)
        '"funcky:types".free "funcky:types".Unit'                                                                                                                                                        || UNIT.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".free $_)'                                                                                                                                           || TRUE.apply(engine)
        '"funcky:commons".equal ("funcky:types".free $_) $_'                                                                                                                                             || FALSE.apply(engine)
    }

    @Unroll('Test unify (expression: #expression')
    def 'Test unify'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                         || result
        '"funcky:types".unify'                                                                                                                                                             || new Types(engine).$unify
        '"funcky:types".type "funcky:types".unify'                                                                                                                                         || FUNCTION(TYPE, TYPE, TYPE).apply(engine)
        '"funcky:types".type ("funcky:types".unify "funcky:types".Type)'                                                                                                                   || FUNCTION(TYPE, TYPE).apply(engine)
        '"funcky:commons".string ("funcky:types".unify ("funcky:commons".error "foo"))'                                                                                                    || engine.converter.convert('"funcky:types".unify ("funcky:commons".error "foo")')
        '"funcky:types".unify "funcky:types".Type "funcky:types".Type'                                                                                                                     || TYPE.apply(engine)
        '"funcky:types".unify "funcky:types".Type $_'                                                                                                                                      || TYPE.apply(engine)
        '"funcky:types".unify "funcky:types".Number "funcky:types".Number'                                                                                                                 || NUMBER.apply(engine)
        '"funcky:types".unify "funcky:types".Number $_'                                                                                                                                    || NUMBER.apply(engine)
        '"funcky:types".unify "funcky:types".Boolean "funcky:types".Boolean'                                                                                                               || BOOLEAN.apply(engine)
        '"funcky:types".unify "funcky:types".Boolean $_'                                                                                                                                   || BOOLEAN.apply(engine)
        '"funcky:types".unify "funcky:types".Character "funcky:types".Character'                                                                                                           || CHARACTER.apply(engine)
        '"funcky:types".unify "funcky:types".Character $_'                                                                                                                                 || CHARACTER.apply(engine)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                     || FUNCTION(TYPE, NUMBER).apply(engine)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function "funcky:types".Type $_)'                                        || FUNCTION(TYPE, NUMBER).apply(engine)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number)'                                      || FUNCTION(TYPE, NUMBER).apply(engine)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function $_ $_)'                                                         || FUNCTION(TYPE, NUMBER).apply(engine)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) $_'                                                                                      || FUNCTION(TYPE, NUMBER).apply(engine)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                        || FUNCTION(TYPE, NUMBER).apply(engine)
        '"funcky:types".domain ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type $_))'                                   || TYPE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type $_)))'      || TRUE.apply(engine)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ "funcky:types".Number)'                                                         || FUNCTION(TYPE, NUMBER).apply(engine)
        '"funcky:types".domain ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ $_))'                                                    || TYPE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ $_)))'                       || TRUE.apply(engine)
        '"funcky:types".domain ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) $_)'                                                                                 || TYPE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) $_))'                                                    || TRUE.apply(engine)
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function "funcky:types".Type $_)'                                                         || FUNCTION(TYPE, NUMBER).apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number)))' || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number))'                                || NUMBER.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ $_)))'                    || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ $_))'                                                   || NUMBER.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) $_))'                                                 || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) $_)'                                                                                || NUMBER.apply(engine)
        '"funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function "funcky:types".Type $_))'                                                    || TYPE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function "funcky:types".Type $_)))'                       || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function $_ "funcky:types".Number)))'                    || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function $_ "funcky:types".Number))'                                                   || NUMBER.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function $_ $_)))'                                       || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function $_ $_)))'                                        || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ $_) $_))'                                                                    || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ $_) $_))'                                                                     || TRUE.apply(engine)
        '"funcky:types".unify ("funcky:types".List "funcky:types".Type) ("funcky:types".List "funcky:types".Type)'                                                                         || LIST(TYPE).apply(engine)
        '"funcky:types".unify ("funcky:types".List "funcky:types".Type) ("funcky:types".List $_)'                                                                                          || LIST(TYPE).apply(engine)
        '"funcky:types".unify ("funcky:types".List "funcky:types".Type) $_'                                                                                                                || LIST(TYPE).apply(engine)
        '"funcky:types".unify ("funcky:types".List $_) ("funcky:types".List "funcky:types".Type)'                                                                                          || LIST(TYPE).apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".unify ("funcky:types".List $_) ("funcky:types".List $_)))'                                                    || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".unify ("funcky:types".List $_) $_))'                                                                          || TRUE.apply(engine)
        '"funcky:types".unify ("funcky:types".List "funcky:types".Character) "funcky:types".String'                                                                                        || STRING.apply(engine)
        '"funcky:types".unify "funcky:types".String ("funcky:types".List "funcky:types".Character)'                                                                                        || STRING.apply(engine)
        '"funcky:types".unify "funcky:types".String "funcky:types".String'                                                                                                                 || STRING.apply(engine)
        '"funcky:types".unify "funcky:types".String $_'                                                                                                                                    || STRING.apply(engine)
        '"funcky:types".unify ("funcky:types".Record []) ("funcky:types".Record [])'                                                                                                       || UNIT.apply(engine)
        '"funcky:types".unify ("funcky:types".Record []) "funcky:types".Unit'                                                                                                              || UNIT.apply(engine)
        '"funcky:types".unify ("funcky:types".Record []) $_'                                                                                                                               || UNIT.apply(engine)
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record ["funcky:types".Type])'                                                                 || RECORD(TYPE).apply(engine)
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record [$_])'                                                                                  || RECORD(TYPE).apply(engine)
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) $_'                                                                                                            || RECORD(TYPE).apply(engine)
        '"funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".Record ["funcky:types".Type])'                                                                                  || RECORD(TYPE).apply(engine)
        '"funcky:types".typeVariable ("funcky:lists".head ("funcky:types".components ("funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".Record [$_]))))'                   || TRUE.apply(engine)
        '"funcky:lists".empty ("funcky:lists".tail ("funcky:types".components ("funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".Record [$_]))))'                          || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:lists".head ("funcky:types".components ("funcky:types".unify ("funcky:types".Record [$_]) $_)))'                                             || TRUE.apply(engine)
        '"funcky:lists".empty ("funcky:lists".tail ("funcky:types".components ("funcky:types".unify ("funcky:types".Record [$_]) $_)))'                                                    || TRUE.apply(engine)
        '"funcky:types".unify "funcky:types".Unit ("funcky:types".Record [])'                                                                                                              || UNIT.apply(engine)
        '"funcky:types".unify "funcky:types".Unit "funcky:types".Unit'                                                                                                                     || UNIT.apply(engine)
        '"funcky:types".unify "funcky:types".Unit $_'                                                                                                                                      || UNIT.apply(engine)
        '"funcky:types".unify $_ "funcky:types".Type'                                                                                                                                      || TYPE.apply(engine)
        '"funcky:types".unify $_ "funcky:types".Number'                                                                                                                                    || NUMBER.apply(engine)
        '"funcky:types".unify $_ "funcky:types".Boolean'                                                                                                                                   || BOOLEAN.apply(engine)
        '"funcky:types".unify $_ "funcky:types".Character'                                                                                                                                 || CHARACTER.apply(engine)
        '"funcky:types".unify $_ ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                                      || FUNCTION(TYPE, NUMBER).apply(engine)
        '"funcky:types".domain ("funcky:types".unify $_ ("funcky:types".Function "funcky:types".Type $_))'                                                                                 || TYPE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify $_ ("funcky:types".Function "funcky:types".Type $_)))'                                                    || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify $_ ("funcky:types".Function $_ "funcky:types".Type)))'                                                   || TRUE.apply(engine)
        '"funcky:types".range ("funcky:types".unify $_ ("funcky:types".Function $_ "funcky:types".Type))'                                                                                  || TYPE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify $_ ("funcky:types".Function $_ $_)))'                                                                    || TRUE.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify $_ ("funcky:types".Function $_ $_)))'                                                                     || TRUE.apply(engine)
        '"funcky:types".unify $_ ("funcky:types".List "funcky:types".Type)'                                                                                                                || LIST(TYPE).apply(engine)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".unify $_ ("funcky:types".List $_)))'                                                                          || TRUE.apply(engine)
        '"funcky:types".unify $_ "funcky:types".String'                                                                                                                                    || STRING.apply(engine)
        '"funcky:types".unify $_ ("funcky:types".Record [])'                                                                                                                               || UNIT.apply(engine)
        '"funcky:types".unify $_ ("funcky:types".Record ["funcky:types".Type])'                                                                                                            || RECORD(TYPE).apply(engine)
        '"funcky:types".typeVariable ("funcky:lists".head ("funcky:types".components ("funcky:types".unify $_ ("funcky:types".Record [$_]))))'                                             || TRUE.apply(engine)
        '"funcky:lists".empty ("funcky:lists".tail ("funcky:types".components ("funcky:types".unify $_ ("funcky:types".Record [$_]))))'                                                    || TRUE.apply(engine)
        '"funcky:types".unify $_ "funcky:types".Unit'                                                                                                                                      || UNIT.apply(engine)
        '"funcky:types".typeVariable ("funcky:types".unify $_ $_)'                                                                                                                         || TRUE.apply(engine)
    }

    @Unroll('Test unify (runtime error, expression: #expression')
    def 'Test unify (runtime error)'(final String expression, final String message) {
        when:
        engine.eval(expression)
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        Pattern.compile(message).matcher(e.message).lookingAt()
        where:
        expression                                                                                                                                                     || message
        '"funcky:types".unify "funcky:types".Type "funcky:types".Number'                                                                                               || 'Can not unify `"funcky\\:types"\\.Type` with `"funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".Type "funcky:types".Boolean'                                                                                              || 'Can not unify `"funcky\\:types"\\.Type` with `"funcky\\:types"\\.Boolean`'
        '"funcky:types".unify "funcky:types".Type "funcky:types".Character'                                                                                            || 'Can not unify `"funcky\\:types"\\.Type` with `"funcky\\:types"\\.Character`'
        '"funcky:types".unify "funcky:types".Type ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                 || 'Can not unify `"funcky\\:types"\\.Type` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type "funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".Type ("funcky:types".Function "funcky:types".Type $_)'                                                                    || 'Can not unify `"funcky\\:types"\\.Type` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Type ("funcky:types".Function $_ "funcky:types".Number)'                                                                  || 'Can not unify `"funcky\\:types"\\.Type` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ "funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".Type ("funcky:types".Function $_ $_)'                                                                                     || 'Can not unify `"funcky\\:types"\\.Type` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Type ("funcky:types".List "funcky:types".Type)'                                                                           || 'Can not unify `"funcky\\:types"\\.Type` with `"funcky\\:types"\\.List "funcky\\:types"\\.Type`'
        '"funcky:types".unify "funcky:types".Type ("funcky:types".List $_)'                                                                                            || 'Can not unify `"funcky\\:types"\\.Type` with `"funcky\\:types"\\.List \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Type "funcky:types".String'                                                                                               || 'Can not unify `"funcky\\:types"\\.Type` with `"funcky\\:types"\\.List "funcky\\:types"\\.Character`'
        '"funcky:types".unify "funcky:types".Type ("funcky:types".Record [])'                                                                                          || 'Can not unify `"funcky\\:types"\\.Type` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify "funcky:types".Type ("funcky:types".Record ["funcky:types".Type])'                                                                       || 'Can not unify `"funcky\\:types"\\.Type` with `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]`'
        '"funcky:types".unify "funcky:types".Type ("funcky:types".Record [$_])'                                                                                        || 'Can not unify `"funcky\\:types"\\.Type` with `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]`'
        '"funcky:types".unify "funcky:types".Type "funcky:types".Unit'                                                                                                 || 'Can not unify `"funcky\\:types"\\.Type` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify "funcky:types".Number "funcky:types".Type'                                                                                               || 'Can not unify `"funcky\\:types"\\.Number` with `"funcky\\:types"\\.Type`'
        '"funcky:types".unify "funcky:types".Number "funcky:types".Boolean'                                                                                            || 'Can not unify `"funcky\\:types"\\.Number` with `"funcky\\:types"\\.Boolean`'
        '"funcky:types".unify "funcky:types".Number "funcky:types".Character'                                                                                          || 'Can not unify `"funcky\\:types"\\.Number` with `"funcky\\:types"\\.Character`'
        '"funcky:types".unify "funcky:types".Number ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                               || 'Can not unify `"funcky\\:types"\\.Number` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type "funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".Number ("funcky:types".Function "funcky:types".Type $_)'                                                                  || 'Can not unify `"funcky\\:types"\\.Number` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Number ("funcky:types".Function $_ "funcky:types".Number)'                                                                || 'Can not unify `"funcky\\:types"\\.Number` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ "funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".Number ("funcky:types".Function $_ $_)'                                                                                   || 'Can not unify `"funcky\\:types"\\.Number` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Number ("funcky:types".List "funcky:types".Type)'                                                                         || 'Can not unify `"funcky\\:types"\\.Number` with `"funcky\\:types"\\.List "funcky\\:types"\\.Type`'
        '"funcky:types".unify "funcky:types".Number ("funcky:types".List $_)'                                                                                          || 'Can not unify `"funcky\\:types"\\.Number` with `"funcky\\:types"\\.List \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Number "funcky:types".String'                                                                                             || 'Can not unify `"funcky\\:types"\\.Number` with `"funcky\\:types"\\.List "funcky\\:types"\\.Character`'
        '"funcky:types".unify "funcky:types".Number ("funcky:types".Record [])'                                                                                        || 'Can not unify `"funcky\\:types"\\.Number` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify "funcky:types".Number ("funcky:types".Record ["funcky:types".Type])'                                                                     || 'Can not unify `"funcky\\:types"\\.Number` with `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]`'
        '"funcky:types".unify "funcky:types".Number ("funcky:types".Record [$_])'                                                                                      || 'Can not unify `"funcky\\:types"\\.Number` with `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]`'
        '"funcky:types".unify "funcky:types".Number "funcky:types".Unit'                                                                                               || 'Can not unify `"funcky\\:types"\\.Number` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify "funcky:types".Boolean "funcky:types".Type'                                                                                              || 'Can not unify `"funcky\\:types"\\.Boolean` with `"funcky\\:types"\\.Type`'
        '"funcky:types".unify "funcky:types".Boolean "funcky:types".Number'                                                                                            || 'Can not unify `"funcky\\:types"\\.Boolean` with `"funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".Boolean "funcky:types".Character'                                                                                         || 'Can not unify `"funcky\\:types"\\.Boolean` with `"funcky\\:types"\\.Character`'
        '"funcky:types".unify "funcky:types".Boolean ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                              || 'Can not unify `"funcky\\:types"\\.Boolean` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type "funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".Boolean ("funcky:types".Function "funcky:types".Type $_)'                                                                 || 'Can not unify `"funcky\\:types"\\.Boolean` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Boolean ("funcky:types".Function $_ "funcky:types".Number)'                                                               || 'Can not unify `"funcky\\:types"\\.Boolean` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ "funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".Boolean ("funcky:types".Function $_ $_)'                                                                                  || 'Can not unify `"funcky\\:types"\\.Boolean` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Boolean ("funcky:types".List "funcky:types".Type)'                                                                        || 'Can not unify `"funcky\\:types"\\.Boolean` with `"funcky\\:types"\\.List "funcky\\:types"\\.Type`'
        '"funcky:types".unify "funcky:types".Boolean ("funcky:types".List $_)'                                                                                         || 'Can not unify `"funcky\\:types"\\.Boolean` with `"funcky\\:types"\\.List \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Boolean "funcky:types".String'                                                                                            || 'Can not unify `"funcky\\:types"\\.Boolean` with `"funcky\\:types"\\.List "funcky\\:types"\\.Character`'
        '"funcky:types".unify "funcky:types".Boolean ("funcky:types".Record [])'                                                                                       || 'Can not unify `"funcky\\:types"\\.Boolean` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify "funcky:types".Boolean ("funcky:types".Record ["funcky:types".Type])'                                                                    || 'Can not unify `"funcky\\:types"\\.Boolean` with `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]`'
        '"funcky:types".unify "funcky:types".Boolean ("funcky:types".Record [$_])'                                                                                     || 'Can not unify `"funcky\\:types"\\.Boolean` with `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]`'
        '"funcky:types".unify "funcky:types".Boolean "funcky:types".Unit'                                                                                              || 'Can not unify `"funcky\\:types"\\.Boolean` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify "funcky:types".Character "funcky:types".Type'                                                                                            || 'Can not unify `"funcky\\:types"\\.Character` with `"funcky\\:types"\\.Type`'
        '"funcky:types".unify "funcky:types".Character "funcky:types".Number'                                                                                          || 'Can not unify `"funcky\\:types"\\.Character` with `"funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".Character "funcky:types".Boolean'                                                                                         || 'Can not unify `"funcky\\:types"\\.Character` with `"funcky\\:types"\\.Boolean`'
        '"funcky:types".unify "funcky:types".Character ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                            || 'Can not unify `"funcky\\:types"\\.Character` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type "funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".Character ("funcky:types".Function "funcky:types".Type $_)'                                                               || 'Can not unify `"funcky\\:types"\\.Character` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Character ("funcky:types".Function $_ "funcky:types".Number)'                                                             || 'Can not unify `"funcky\\:types"\\.Character` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ "funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".Character ("funcky:types".Function $_ $_)'                                                                                || 'Can not unify `"funcky\\:types"\\.Character` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Character ("funcky:types".List "funcky:types".Type)'                                                                      || 'Can not unify `"funcky\\:types"\\.Character` with `"funcky\\:types"\\.List "funcky\\:types"\\.Type`'
        '"funcky:types".unify "funcky:types".Character ("funcky:types".List $_)'                                                                                       || 'Can not unify `"funcky\\:types"\\.Character` with `"funcky\\:types"\\.List \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Character "funcky:types".String'                                                                                          || 'Can not unify `"funcky\\:types"\\.Character` with `"funcky\\:types"\\.List "funcky\\:types"\\.Character`'
        '"funcky:types".unify "funcky:types".Character ("funcky:types".Record [])'                                                                                     || 'Can not unify `"funcky\\:types"\\.Character` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify "funcky:types".Character ("funcky:types".Record ["funcky:types".Type])'                                                                  || 'Can not unify `"funcky\\:types"\\.Character` with `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]`'
        '"funcky:types".unify "funcky:types".Character ("funcky:types".Record [$_])'                                                                                   || 'Can not unify `"funcky\\:types"\\.Character` with `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]`'
        '"funcky:types".unify "funcky:types".Character "funcky:types".Unit'                                                                                            || 'Can not unify `"funcky\\:types"\\.Character` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number "funcky:types".Type) "funcky:types".Type'                                                 || 'Can not unify `"funcky:types".Function "funcky:types".Number "funcky:types".Type` with `"funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number "funcky:types".Type) "funcky:types".Number'                                               || 'Can not unify `"funcky:types".Function "funcky:types".Number "funcky:types".Type` with `"funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number "funcky:types".Type) "funcky:types".Boolean'                                              || 'Can not unify `"funcky:types".Function "funcky:types".Number "funcky:types".Type` with `"funcky\\:types"\\.Boolean`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number "funcky:types".Type) "funcky:types".Character'                                            || 'Can not unify `"funcky:types".Function "funcky:types".Number "funcky:types".Type` with `"funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number "funcky:types".Type) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || 'Can not unify `"funcky:types".Function "funcky:types".Number "funcky:types".Type` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number "funcky:types".Type) ("funcky:types".Function "funcky:types".Type $_)'                    || 'Can not unify `"funcky:types".Function "funcky:types".Number "funcky:types".Type` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number "funcky:types".Type) ("funcky:types".Function $_ "funcky:types".Number)'                  || 'Can not unify `"funcky:types".Function "funcky:types".Number "funcky:types".Type` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number "funcky:types".Type) ("funcky:types".List "funcky:types".Type)'                           || 'Can not unify `"funcky:types".Function "funcky:types".Number "funcky:types".Type` with `"funcky\\:types"\\.List "funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number "funcky:types".Type) ("funcky:types".List $_)'                                            || 'Can not unify `"funcky:types".Function "funcky:types".Number "funcky:types".Type` with `"funcky\\:types"\\.List \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number "funcky:types".Type) "funcky:types".String'                                               || 'Can not unify `"funcky:types".Function "funcky:types".Number "funcky:types".Type` with `"funcky\\:types"\\.List "funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number "funcky:types".Type) ("funcky:types".Record [])'                                          || 'Can not unify `"funcky:types".Function "funcky:types".Number "funcky:types".Type` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number "funcky:types".Type) ("funcky:types".Record ["funcky:types".Type])'                       || 'Can not unify `"funcky:types".Function "funcky:types".Number "funcky:types".Type` with `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number "funcky:types".Type) ("funcky:types".Record [$_])'                                        || 'Can not unify `"funcky:types".Function "funcky:types".Number "funcky:types".Type` with `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number "funcky:types".Type) "funcky:types".Unit'                                                 || 'Can not unify `"funcky:types".Function "funcky:types".Number "funcky:types".Type` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number $_) "funcky:types".Type'                                                                  || 'Can not unify `"funcky:types".Function "funcky:types".Number \\$_[0-9a-f]+` with `"funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number $_) "funcky:types".Number'                                                                || 'Can not unify `"funcky:types".Function "funcky:types".Number \\$_[0-9a-f]+` with `"funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number $_) "funcky:types".Boolean'                                                               || 'Can not unify `"funcky:types".Function "funcky:types".Number \\$_[0-9a-f]+` with `"funcky\\:types"\\.Boolean`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number $_) "funcky:types".Character'                                                             || 'Can not unify `"funcky:types".Function "funcky:types".Number \\$_[0-9a-f]+` with `"funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number $_) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                  || 'Can not unify `"funcky:types".Function "funcky:types".Number \\$_[0-9a-f]+` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number $_) ("funcky:types".Function "funcky:types".Type $_)'                                     || 'Can not unify `"funcky:types".Function "funcky:types".Number \\$_[0-9a-f]+` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number $_) ("funcky:types".List "funcky:types".Type)'                                            || 'Can not unify `"funcky:types".Function "funcky:types".Number \\$_[0-9a-f]+` with `"funcky\\:types"\\.List "funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number $_) ("funcky:types".List $_)'                                                             || 'Can not unify `"funcky:types".Function "funcky:types".Number \\$_[0-9a-f]+` with `"funcky\\:types"\\.List \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number $_) "funcky:types".String'                                                                || 'Can not unify `"funcky:types".Function "funcky:types".Number \\$_[0-9a-f]+` with `"funcky\\:types"\\.List "funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number $_) ("funcky:types".Record [])'                                                           || 'Can not unify `"funcky:types".Function "funcky:types".Number \\$_[0-9a-f]+` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number $_) ("funcky:types".Record ["funcky:types".Type])'                                        || 'Can not unify `"funcky:types".Function "funcky:types".Number \\$_[0-9a-f]+` with `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number $_) ("funcky:types".Record [$_])'                                                         || 'Can not unify `"funcky:types".Function "funcky:types".Number \\$_[0-9a-f]+` with `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]`'
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Number $_) "funcky:types".Unit'                                                                  || 'Can not unify `"funcky:types".Function "funcky:types".Number \\$_[0-9a-f]+` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Type) "funcky:types".Type'                                                                    || 'Can not unify `"funcky:types".Function \\$_[0-9a-f]+ "funcky:types".Type` with `"funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Type) "funcky:types".Number'                                                                  || 'Can not unify `"funcky:types".Function \\$_[0-9a-f]+ "funcky:types".Type` with `"funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Type) "funcky:types".Boolean'                                                                 || 'Can not unify `"funcky:types".Function \\$_[0-9a-f]+ "funcky:types".Type` with `"funcky\\:types"\\.Boolean`'
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Type) "funcky:types".Character'                                                               || 'Can not unify `"funcky:types".Function \\$_[0-9a-f]+ "funcky:types".Type` with `"funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Type) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                    || 'Can not unify `"funcky:types".Function \\$_[0-9a-f]+ "funcky:types".Type` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Type) ("funcky:types".Function $_ "funcky:types".Number)'                                     || 'Can not unify `"funcky:types".Function \\$_[0-9a-f]+ "funcky:types".Type` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Type) ("funcky:types".List "funcky:types".Type)'                                              || 'Can not unify `"funcky:types".Function \\$_[0-9a-f]+ "funcky:types".Type` with `"funcky\\:types"\\.List "funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Type) ("funcky:types".List $_)'                                                               || 'Can not unify `"funcky:types".Function \\$_[0-9a-f]+ "funcky:types".Type` with `"funcky\\:types"\\.List \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Type) "funcky:types".String'                                                                  || 'Can not unify `"funcky:types".Function \\$_[0-9a-f]+ "funcky:types".Type` with `"funcky\\:types"\\.List "funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Type) ("funcky:types".Record [])'                                                             || 'Can not unify `"funcky:types".Function \\$_[0-9a-f]+ "funcky:types".Type` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Type) ("funcky:types".Record ["funcky:types".Type])'                                          || 'Can not unify `"funcky:types".Function \\$_[0-9a-f]+ "funcky:types".Type` with `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]`'
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Type) ("funcky:types".Record [$_])'                                                           || 'Can not unify `"funcky:types".Function \\$_[0-9a-f]+ "funcky:types".Type` with `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]`'
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Type) "funcky:types".Unit'                                                                    || 'Can not unify `"funcky:types".Function \\$_[0-9a-f]+ "funcky:types".Type` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".Function $_ $_) "funcky:types".Type'                                                                                     || 'Can not unify `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+` with `"funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Function $_ $_) "funcky:types".Number'                                                                                   || 'Can not unify `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+` with `"funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Function $_ $_) "funcky:types".Boolean'                                                                                  || 'Can not unify `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+` with `"funcky\\:types"\\.Boolean`'
        '"funcky:types".unify ("funcky:types".Function $_ $_) "funcky:types".Character'                                                                                || 'Can not unify `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+` with `"funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".List "funcky:types".Type)'                                                               || 'Can not unify `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+` with `"funcky\\:types"\\.List "funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".List $_)'                                                                                || 'Can not unify `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+` with `"funcky\\:types"\\.List \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Function $_ $_) "funcky:types".String'                                                                                   || 'Can not unify `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+` with `"funcky\\:types"\\.List "funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Record [])'                                                                              || 'Can not unify `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Record ["funcky:types".Type])'                                                           || 'Can not unify `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+` with `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]`'
        '"funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Record [$_])'                                                                            || 'Can not unify `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+` with `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]`'
        '"funcky:types".unify ("funcky:types".Function $_ $_) "funcky:types".Unit'                                                                                     || 'Can not unify `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".List "funcky:types".Number) "funcky:types".Type'                                                                         || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Number` with `"funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".List "funcky:types".Number) "funcky:types".Number'                                                                       || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Number` with `"funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".List "funcky:types".Number) "funcky:types".Boolean'                                                                      || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Number` with `"funcky\\:types"\\.Boolean`'
        '"funcky:types".unify ("funcky:types".List "funcky:types".Number) "funcky:types".Character'                                                                    || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Number` with `"funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".List "funcky:types".Number) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                         || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Number` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".List "funcky:types".Number) ("funcky:types".Function "funcky:types".Type $_)'                                            || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Number` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".List "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number)'                                          || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Number` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".List "funcky:types".Number) ("funcky:types".Function $_ $_)'                                                             || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Number` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".List "funcky:types".Number) ("funcky:types".List "funcky:types".Type)'                                                   || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Number` with `"funcky\\:types"\\.List "funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".List "funcky:types".Number) "funcky:types".String'                                                                       || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Number` with `"funcky\\:types"\\.List "funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".List "funcky:types".Number) ("funcky:types".Record [])'                                                                  || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Number` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".List "funcky:types".Number) ("funcky:types".Record ["funcky:types".Type])'                                               || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Number` with `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]`'
        '"funcky:types".unify ("funcky:types".List "funcky:types".Number) ("funcky:types".Record [$_])'                                                                || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Number` with `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]`'
        '"funcky:types".unify ("funcky:types".List "funcky:types".Number) "funcky:types".Unit'                                                                         || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Number` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".List $_) "funcky:types".Type'                                                                                            || 'Can not unify `"funcky\\:types"\\.List \\$_[0-9a-f]+` with `"funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".List $_) "funcky:types".Number'                                                                                          || 'Can not unify `"funcky\\:types"\\.List \\$_[0-9a-f]+` with `"funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".List $_) "funcky:types".Boolean'                                                                                         || 'Can not unify `"funcky\\:types"\\.List \\$_[0-9a-f]+` with `"funcky\\:types"\\.Boolean`'
        '"funcky:types".unify ("funcky:types".List $_) "funcky:types".Character'                                                                                       || 'Can not unify `"funcky\\:types"\\.List \\$_[0-9a-f]+` with `"funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".List $_) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                            || 'Can not unify `"funcky\\:types"\\.List \\$_[0-9a-f]+` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".List $_) ("funcky:types".Function "funcky:types".Type $_)'                                                               || 'Can not unify `"funcky\\:types"\\.List \\$_[0-9a-f]+` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".List $_) ("funcky:types".Function $_ "funcky:types".Number)'                                                             || 'Can not unify `"funcky\\:types"\\.List \\$_[0-9a-f]+` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".List $_) ("funcky:types".Function $_ $_)'                                                                                || 'Can not unify `"funcky\\:types"\\.List \\$_[0-9a-f]+` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".List $_) ("funcky:types".Record [])'                                                                                     || 'Can not unify `"funcky\\:types"\\.List \\$_[0-9a-f]+` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".List $_) ("funcky:types".Record ["funcky:types".Type])'                                                                  || 'Can not unify `"funcky\\:types"\\.List \\$_[0-9a-f]+` with `"funcky\\:types"\\.Record \\["funcky:types".Type\\]`'
        '"funcky:types".unify ("funcky:types".List $_) ("funcky:types".Record [$_])'                                                                                   || 'Can not unify `"funcky\\:types"\\.List \\$_[0-9a-f]+` with `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]`'
        '"funcky:types".unify ("funcky:types".List $_) "funcky:types".Unit'                                                                                            || 'Can not unify `"funcky\\:types"\\.List \\$_[0-9a-f]+` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify "funcky:types".String "funcky:types".Type'                                                                                               || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Character` with `"funcky\\:types"\\.Type`'
        '"funcky:types".unify "funcky:types".String "funcky:types".Number'                                                                                             || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Character` with `"funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".String "funcky:types".Boolean'                                                                                            || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Character` with `"funcky\\:types"\\.Boolean`'
        '"funcky:types".unify "funcky:types".String "funcky:types".Character'                                                                                          || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Character` with `"funcky\\:types"\\.Character`'
        '"funcky:types".unify "funcky:types".String ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                               || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Character` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type "funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".String ("funcky:types".Function "funcky:types".Type $_)'                                                                  || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Character` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".String ("funcky:types".Function $_ "funcky:types".Number)'                                                                || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Character` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ "funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".String ("funcky:types".Function $_ $_)'                                                                                   || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Character` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".String ("funcky:types".List "funcky:types".Type)'                                                                         || 'Can not unify `"funcky\\:types"\\.List "funcky\\:types"\\.Character` with `"funcky\\:types"\\.List "funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Record []) "funcky:types".Type'                                                                                          || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Record []) "funcky:types".Number'                                                                                        || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Record []) "funcky:types".Boolean'                                                                                       || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Boolean`'
        '"funcky:types".unify ("funcky:types".Record []) "funcky:types".Character'                                                                                     || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".Record []) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                          || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Record []) ("funcky:types".Function "funcky:types".Type $_)'                                                             || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Record []) ("funcky:types".Function $_ "funcky:types".Number)'                                                           || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Record []) ("funcky:types".Function $_ $_)'                                                                              || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Record []) ("funcky:types".List "funcky:types".Type)'                                                                    || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.List "funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Record []) ("funcky:types".List $_)'                                                                                     || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.List \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Record []) "funcky:types".String'                                                                                        || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.List "funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".Record []) ("funcky:types".Record ["funcky:types".Type])'                                                                || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]`'
        '"funcky:types".unify ("funcky:types".Record []) ("funcky:types".Record [$_])'                                                                                 || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]`'
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Type'                                                                       || 'Can not unify `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]` with `"funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Number'                                                                     || 'Can not unify `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]` with `"funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Boolean'                                                                    || 'Can not unify `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]` with `"funcky\\:types"\\.Boolean`'
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Character'                                                                  || 'Can not unify `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]` with `"funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                       || 'Can not unify `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function "funcky:types".Type $_)'                                          || 'Can not unify `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function $_ "funcky:types".Number)'                                        || 'Can not unify `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Function $_ $_)'                                                           || 'Can not unify `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".List "funcky:types".Type)'                                                 || 'Can not unify `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]` with `"funcky\\:types"\\.List "funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".List $_)'                                                                  || 'Can not unify `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]` with `"funcky\\:types"\\.List \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) "funcky:types".String'                                                                     || 'Can not unify `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]` with `"funcky\\:types"\\.List "funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record [])'                                                                || 'Can not unify `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record ["funcky:types".Number])'                                           || 'Can not unify `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]` with `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Number\\]`'
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) "funcky:types".Unit'                                                                       || 'Can not unify `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".Record [$_]) "funcky:types".Type'                                                                                        || 'Can not unify `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]` with `"funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Record [$_]) "funcky:types".Number'                                                                                      || 'Can not unify `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]` with `"funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Record [$_]) "funcky:types".Boolean'                                                                                     || 'Can not unify `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]` with `"funcky\\:types"\\.Boolean`'
        '"funcky:types".unify ("funcky:types".Record [$_]) "funcky:types".Character'                                                                                   || 'Can not unify `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]` with `"funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                        || 'Can not unify `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".Function "funcky:types".Type $_)'                                                           || 'Can not unify `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".Function $_ "funcky:types".Number)'                                                         || 'Can not unify `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ "funcky\\:types"\\.Number`'
        '"funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".Function $_ $_)'                                                                            || 'Can not unify `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".List "funcky:types".Type)'                                                                  || 'Can not unify `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]` with `"funcky\\:types"\\.List "funcky\\:types"\\.Type`'
        '"funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".List $_)'                                                                                   || 'Can not unify `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]` with `"funcky\\:types"\\.List \\$_[0-9a-f]+`'
        '"funcky:types".unify ("funcky:types".Record [$_]) "funcky:types".String'                                                                                      || 'Can not unify `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]` with `"funcky\\:types"\\.List "funcky\\:types"\\.Character`'
        '"funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".Record [])'                                                                                 || 'Can not unify `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify ("funcky:types".Record [$_]) "funcky:types".Unit'                                                                                        || 'Can not unify `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]` with `"funcky\\:types"\\.Record \\[\\]`'
        '"funcky:types".unify "funcky:types".Unit "funcky:types".Type'                                                                                                 || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Type`'
        '"funcky:types".unify "funcky:types".Unit "funcky:types".Number'                                                                                               || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".Unit "funcky:types".Boolean'                                                                                              || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Boolean`'
        '"funcky:types".unify "funcky:types".Unit "funcky:types".Character'                                                                                            || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Character`'
        '"funcky:types".unify "funcky:types".Unit ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                 || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type "funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".Unit ("funcky:types".Function "funcky:types".Type $_)'                                                                    || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Function "funcky\\:types"\\.Type \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Unit ("funcky:types".Function $_ "funcky:types".Number)'                                                                  || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ "funcky\\:types"\\.Number`'
        '"funcky:types".unify "funcky:types".Unit ("funcky:types".Function $_ $_)'                                                                                     || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Function \\$_[0-9a-f]+ \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Unit ("funcky:types".List "funcky:types".Type)'                                                                           || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.List "funcky\\:types"\\.Type`'
        '"funcky:types".unify "funcky:types".Unit ("funcky:types".List $_)'                                                                                            || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.List \\$_[0-9a-f]+`'
        '"funcky:types".unify "funcky:types".Unit "funcky:types".String'                                                                                               || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.List "funcky\\:types"\\.Character`'
        '"funcky:types".unify "funcky:types".Unit ("funcky:types".Record ["funcky:types".Type])'                                                                       || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Record \\["funcky\\:types"\\.Type\\]`'
        '"funcky:types".unify "funcky:types".Unit ("funcky:types".Record [$_])'                                                                                        || 'Can not unify `"funcky\\:types"\\.Record \\[\\]` with `"funcky\\:types"\\.Record \\[\\$_[0-9a-f]+\\]`'
    }
}
