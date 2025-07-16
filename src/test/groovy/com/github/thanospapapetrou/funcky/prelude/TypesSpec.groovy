package com.github.thanospapapetrou.funcky.prelude

import java.util.regex.Pattern

import com.github.thanospapapetrou.funcky.BaseSpec
import com.github.thanospapapetrou.funcky.runtime.FunckyBoolean
import com.github.thanospapapetrou.funcky.runtime.FunckyList
import com.github.thanospapapetrou.funcky.runtime.FunckyValue
import com.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException
import com.github.thanospapapetrou.funcky.runtime.prelude.Types
import com.github.thanospapapetrou.funcky.runtime.types.FunckyFunctionType
import com.github.thanospapapetrou.funcky.runtime.types.FunckyListType
import com.github.thanospapapetrou.funcky.runtime.types.FunckyRecordType
import com.github.thanospapapetrou.funcky.runtime.types.FunckySimpleType
import spock.lang.Unroll

class TypesSpec extends BaseSpec {
    @Unroll('Test simple types (expression: #expression)')
    def 'Test simple types'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                     || result
        '"funcky:types".Type'                          || FunckySimpleType.TYPE
        '"funcky:types".type "funcky:types".Type'      || FunckySimpleType.TYPE
        '"funcky:types".Number'                        || FunckySimpleType.NUMBER
        '"funcky:types".type "funcky:types".Number'    || FunckySimpleType.TYPE
        '"funcky:types".Boolean'                       || FunckySimpleType.BOOLEAN
        '"funcky:types".type "funcky:types".Boolean'   || FunckySimpleType.TYPE
        '"funcky:types".Character'                     || FunckySimpleType.CHARACTER
        '"funcky:types".type "funcky:types".Character' || FunckySimpleType.TYPE
    }

    @Unroll('Test Function (expression: #expression)')
    def 'Test Function'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                             || result
        '"funcky:types".Function'                                                                              || Types.FUNCTION
        '"funcky:types".type "funcky:types".Function'                                                          || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.TYPE, FunckySimpleType.TYPE)
        '"funcky:types".type ("funcky:types".Function "funcky:types".Type)'                                    || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.TYPE)
        '"funcky:commons".string ("funcky:types".Function ("funcky:commons".error "foo"))'                     || engine.converter.convert('"funcky:types".Function ("funcky:commons".error "foo")')
        '"funcky:types".Function "funcky:types".Type "funcky:types".Number'                                    || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.NUMBER)
        '"funcky:commons".string ("funcky:types".Function "funcky:types".Type ("funcky:commons".error "foo"))' || engine.converter.convert('"funcky:types".Function "funcky:types".Type ("funcky:commons".error "foo")')
    }

    @Unroll('Test domain (expression: #expression)')
    def 'Test domain'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                  || result
        '"funcky:types".domain'                                                                     || Types.DOMAIN
        '"funcky:types".type "funcky:types".domain'                                                 || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.TYPE)
        '"funcky:types".domain ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || FunckySimpleType.TYPE
    }

    def 'Test domain (runtime error)'() {
        when:
        engine.eval('"funcky:types".domain "funcky:types".Type')
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(String.format(Types.ERROR_DOMAIN, FunckySimpleType.TYPE))
    }

    @Unroll('Test range (expression: #expression)')
    def 'Test range'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                 || result
        '"funcky:types".range'                                                                     || Types.RANGE
        '"funcky:types".type "funcky:types".range'                                                 || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.TYPE)
        '"funcky:types".range ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || FunckySimpleType.NUMBER
    }

    def 'Test range (runtime error)'() {
        when:
        engine.eval('"funcky:types".range "funcky:types".Type')
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(String.format(Types.ERROR_RANGE, FunckySimpleType.TYPE))
    }

    @Unroll('Test List (expression: #expression)')
    def 'Test List'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                     || result
        '"funcky:types".List'                                                          || Types.LIST
        '"funcky:types".type "funcky:types".List'                                      || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.TYPE)
        '"funcky:types".List "funcky:types".Type'                                      || new FunckyListType(FunckySimpleType.TYPE)
        '"funcky:commons".string ("funcky:types".List ("funcky:commons".error "foo"))' || engine.converter.convert('"funcky:types".List ("funcky:commons".error "foo")')
    }

    @Unroll('Test element (expression: #expression)')
    def 'Test element'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                         || result
        '"funcky:types".element'                                           || Types.ELEMENT
        '"funcky:types".type "funcky:types".element'                       || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.TYPE)
        '"funcky:types".element ("funcky:types".List "funcky:types".Type)' || FunckySimpleType.TYPE
    }

    def 'Test element (runtime error)'() {
        when:
        engine.eval('"funcky:types".element "funcky:types".Type')
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(String.format(Types.ERROR_ELEMENT, FunckySimpleType.TYPE))
    }

    @Unroll('Test String (expression: #expression)')
    def 'Test String'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                  || result
        '"funcky:types".String'                     || FunckyListType.STRING
        '"funcky:types".type "funcky:types".String' || FunckySimpleType.TYPE

    }

    @Unroll('Test Record (expression: #expression)')
    def 'Test Record'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                    || result
        '"funcky:types".Record'                       || Types.RECORD
        '"funcky:types".type "funcky:types".Record'   || new FunckyFunctionType(new FunckyListType(FunckySimpleType.TYPE), FunckySimpleType.TYPE)
        '"funcky:types".Record []'                    || new FunckyRecordType(new FunckyList(new FunckyListType(FunckySimpleType.TYPE), null, null))
        '"funcky:types".Record ["funcky:types".Type]' || new FunckyRecordType(new FunckyList(new FunckyListType(FunckySimpleType.TYPE), FunckySimpleType.TYPE, new FunckyList(new FunckyListType(new FunckyListType(FunckySimpleType.TYPE)), null, null)))
    }

    @Unroll('Test components (expression: #expression)')
    def 'Test components'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                || result
        '"funcky:types".components'                                               || Types.COMPONENTS
        '"funcky:types".type "funcky:types".components'                           || new FunckyFunctionType(FunckySimpleType.TYPE, new FunckyListType(FunckySimpleType.TYPE))
        '"funcky:types".components ("funcky:types".Record [])'                    || engine.converter.convert([])
        '"funcky:types".components ("funcky:types".Record ["funcky:types".Type])' || engine.converter.convert([FunckySimpleType.TYPE])
    }

    def 'Test components (runtime error)'() {
        when:
        engine.eval('"funcky:types".components "funcky:types".Type')
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(String.format(Types.ERROR_COMPONENTS, FunckySimpleType.TYPE))
    }

    @Unroll('Test Unit (expression: #expression)')
    def 'Test Unit'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                || result
        '"funcky:types".Unit'                     || new FunckyRecordType(engine.converter.convert(([])))
        '"funcky:types".type "funcky:types".Unit' || FunckySimpleType.TYPE
    }

    @Unroll('Test type (expression: #expression)')
    def 'Test type'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                      || result
        '"funcky:types".type'                                                                           || Types.TYPE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:types".type))' || FunckyBoolean.TRUE
        '"funcky:types".type "funcky:types".Type'                                                       || FunckySimpleType.TYPE
        '"funcky:types".type 1'                                                                         || FunckySimpleType.NUMBER
        '"funcky:types".type "funcky:booleans".false'                                                   || FunckySimpleType.BOOLEAN
        '"funcky:types".type \'a\''                                                                     || FunckySimpleType.CHARACTER
        '"funcky:types".type "funcky:types".free'                                                       || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.TYPE)
        '"funcky:types".listType ("funcky:types".type [])'                                              || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".type []))'                 || FunckyBoolean.TRUE
        '"funcky:types".type [1]'                                                                       || new FunckyListType(FunckySimpleType.NUMBER)
        '"funcky:types".type ""'                                                                        || FunckyListType.STRING
        '"funcky:types".type {}'                                                                        || FunckyRecordType.UNIT
        '"funcky:types".type {1}'                                                                       || new FunckyRecordType(engine.converter.convert([FunckySimpleType.NUMBER]))
        '"funcky:types".type {1, \'a\'}'                                                                || new FunckyRecordType(engine.converter.convert([FunckySimpleType.NUMBER, FunckySimpleType.CHARACTER]))
    }

    @Unroll('Test typeVariable (expression: #expression)')
    def 'Test typeVariable'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                        || result
        '"funcky:types".typeVariable'                                                                     || Types.TYPE_VARIABLE
        '"funcky:types".type "funcky:types".typeVariable'                                                 || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN)
        '"funcky:types".typeVariable "funcky:types".Type'                                                 || FunckyBoolean.FALSE
        '"funcky:types".typeVariable "funcky:types".Number'                                               || FunckyBoolean.FALSE
        '"funcky:types".typeVariable "funcky:types".Boolean'                                              || FunckyBoolean.FALSE
        '"funcky:types".typeVariable "funcky:types".Character'                                            || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".List "funcky:types".Type)'                           || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".String)'                                             || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".Record [])'                                          || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".Record ["funcky:types".Type])'                       || FunckyBoolean.FALSE
        '"funcky:types".typeVariable "funcky:types".Unit'                                                 || FunckyBoolean.FALSE
        '"funcky:types".typeVariable $_'                                                                  || FunckyBoolean.TRUE
    }

    @Unroll('Test functionType (expression: #expression)')
    def 'Test functionType'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                        || result
        '"funcky:types".functionType'                                                                     || Types.FUNCTION_TYPE
        '"funcky:types".type "funcky:types".functionType'                                                 || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN)
        '"funcky:types".functionType "funcky:types".Type'                                                 || FunckyBoolean.FALSE
        '"funcky:types".functionType "funcky:types".Number'                                               || FunckyBoolean.FALSE
        '"funcky:types".functionType "funcky:types".Boolean'                                              || FunckyBoolean.FALSE
        '"funcky:types".functionType "funcky:types".Character'                                            || FunckyBoolean.FALSE
        '"funcky:types".functionType ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || FunckyBoolean.TRUE
        '"funcky:types".functionType ("funcky:types".List "funcky:types".Type)'                           || FunckyBoolean.FALSE
        '"funcky:types".functionType ("funcky:types".String)'                                             || FunckyBoolean.FALSE
        '"funcky:types".functionType ("funcky:types".Record [])'                                          || FunckyBoolean.FALSE
        '"funcky:types".functionType ("funcky:types".Record ["funcky:types".Type])'                       || FunckyBoolean.FALSE
        '"funcky:types".functionType "funcky:types".Unit'                                                 || FunckyBoolean.FALSE
        '"funcky:types".functionType $_'                                                                  || FunckyBoolean.FALSE
    }

    @Unroll('Test listType (expression: #expression)')
    def 'Test listType'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                    || result
        '"funcky:types".listType'                                                                     || Types.LIST_TYPE
        '"funcky:types".type "funcky:types".listType'                                                 || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN)
        '"funcky:types".listType "funcky:types".Type'                                                 || FunckyBoolean.FALSE
        '"funcky:types".listType "funcky:types".Number'                                               || FunckyBoolean.FALSE
        '"funcky:types".listType "funcky:types".Boolean'                                              || FunckyBoolean.FALSE
        '"funcky:types".listType "funcky:types".Character'                                            || FunckyBoolean.FALSE
        '"funcky:types".listType ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || FunckyBoolean.FALSE
        '"funcky:types".listType ("funcky:types".List "funcky:types".Type)'                           || FunckyBoolean.TRUE
        '"funcky:types".listType ("funcky:types".String)'                                             || FunckyBoolean.TRUE
        '"funcky:types".listType ("funcky:types".Record [])'                                          || FunckyBoolean.FALSE
        '"funcky:types".listType ("funcky:types".Record ["funcky:types".Type])'                       || FunckyBoolean.FALSE
        '"funcky:types".listType "funcky:types".Unit'                                                 || FunckyBoolean.FALSE
        '"funcky:types".listType $_'                                                                  || FunckyBoolean.FALSE
    }

    @Unroll('Test recordType (expression: #expression)')
    def 'Test recordType'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                      || result
        '"funcky:types".recordType'                                                                     || Types.RECORD_TYPE
        '"funcky:types".type "funcky:types".recordType'                                                 || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.BOOLEAN)
        '"funcky:types".recordType "funcky:types".Type'                                                 || FunckyBoolean.FALSE
        '"funcky:types".recordType "funcky:types".Number'                                               || FunckyBoolean.FALSE
        '"funcky:types".recordType "funcky:types".Boolean'                                              || FunckyBoolean.FALSE
        '"funcky:types".recordType "funcky:types".Character'                                            || FunckyBoolean.FALSE
        '"funcky:types".recordType ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || FunckyBoolean.FALSE
        '"funcky:types".recordType ("funcky:types".List "funcky:types".Type)'                           || FunckyBoolean.FALSE
        '"funcky:types".recordType ("funcky:types".String)'                                             || FunckyBoolean.FALSE
        '"funcky:types".recordType ("funcky:types".Record [])'                                          || FunckyBoolean.TRUE
        '"funcky:types".recordType ("funcky:types".Record ["funcky:types".Type])'                       || FunckyBoolean.TRUE
        '"funcky:types".recordType "funcky:types".Unit'                                                 || FunckyBoolean.TRUE
        '"funcky:types".recordType $_'                                                                  || FunckyBoolean.FALSE
    }

    @Unroll('Test free (expression: #expression')
    def 'Test free'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                       || result
        '"funcky:types".free'                                                                                                                                                                            || Types.FREE
        '"funcky:types".type "funcky:types".free'                                                                                                                                                        || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.TYPE)
        '"funcky:types".free "funcky:types".Type'                                                                                                                                                        || FunckySimpleType.TYPE
        '"funcky:types".free "funcky:types".Number'                                                                                                                                                      || FunckySimpleType.NUMBER
        '"funcky:types".free "funcky:types".Boolean'                                                                                                                                                     || FunckySimpleType.BOOLEAN
        '"funcky:types".free "funcky:types".Character'                                                                                                                                                   || FunckySimpleType.CHARACTER
        '"funcky:types".free ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                                                        || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.NUMBER)
        '"funcky:types".domain ("funcky:types".free ("funcky:types".Function "funcky:types".Type $_))'                                                                                                   || FunckySimpleType.TYPE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".free ("funcky:types".Function "funcky:types".Type $_)))'                                                                      || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".free ("funcky:types".Function "funcky:types".Type $_))) ("funcky:types".range ("funcky:types".Function "funcky:types".Type $_))'   || FunckyBoolean.FALSE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".free ("funcky:types".Function $_ "funcky:types".Type)))'                                                                     || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".free ("funcky:types".Function $_ "funcky:types".Type))) ("funcky:types".domain ("funcky:types".Function $_ "funcky:types".Type))' || FunckyBoolean.FALSE
        '"funcky:types".range ("funcky:types".free ("funcky:types".Function $_ "funcky:types".Type))'                                                                                                    || FunckySimpleType.TYPE
        '"funcky:types".free ("funcky:types".List "funcky:types".Type)'                                                                                                                                  || new FunckyListType(FunckySimpleType.TYPE)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".free ("funcky:types".List $_)))'                                                                                            || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".free ("funcky:types".List $_))) ("funcky:types".element ("funcky:types".List $_))'                                               || FunckyBoolean.FALSE
        '"funcky:types".free "funcky:types".String'                                                                                                                                                      || FunckyListType.STRING
        '"funcky:types".free ("funcky:types".Record [])'                                                                                                                                                 || FunckyRecordType.UNIT
        '"funcky:types".free ("funcky:types".Record ["funcky:types".Type])'                                                                                                                              || new FunckyRecordType(engine.converter.convert([FunckySimpleType.TYPE]))
        '"funcky:types".typeVariable ("funcky:lists".head ("funcky:types".components ("funcky:types".free ("funcky:types".Record [$_]))))'                                                               || FunckyBoolean.TRUE
        '"funcky:lists".empty ("funcky:lists".tail ("funcky:types".components ("funcky:types".free ("funcky:types".Record [$_]))))'                                                                      || FunckyBoolean.TRUE
        '"funcky:types".free "funcky:types".Unit'                                                                                                                                                        || FunckyRecordType.UNIT
        '"funcky:types".typeVariable ("funcky:types".free $_)'                                                                                                                                           || FunckyBoolean.TRUE
        '"funcky:commons".equal ("funcky:types".free $_) $_'                                                                                                                                             || FunckyBoolean.FALSE
    }

    @Unroll('Test unify (expression: #expression')
    def 'Test unify'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                         || result
        '"funcky:types".unify'                                                                                                                                                             || Types.UNIFY
        '"funcky:types".type "funcky:types".unify'                                                                                                                                         || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.TYPE, FunckySimpleType.TYPE)
        '"funcky:types".type ("funcky:types".unify "funcky:types".Type)'                                                                                                                   || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.TYPE)
        '"funcky:commons".string ("funcky:types".unify ("funcky:commons".error "foo"))'                                                                                                    || engine.converter.convert('"funcky:types".unify ("funcky:commons".error "foo")')
        '"funcky:types".unify "funcky:types".Type "funcky:types".Type'                                                                                                                     || FunckySimpleType.TYPE
        '"funcky:types".unify "funcky:types".Type $_'                                                                                                                                      || FunckySimpleType.TYPE
        '"funcky:types".unify "funcky:types".Number "funcky:types".Number'                                                                                                                 || FunckySimpleType.NUMBER
        '"funcky:types".unify "funcky:types".Number $_'                                                                                                                                    || FunckySimpleType.NUMBER
        '"funcky:types".unify "funcky:types".Boolean "funcky:types".Boolean'                                                                                                               || FunckySimpleType.BOOLEAN
        '"funcky:types".unify "funcky:types".Boolean $_'                                                                                                                                   || FunckySimpleType.BOOLEAN
        '"funcky:types".unify "funcky:types".Character "funcky:types".Character'                                                                                                           || FunckySimpleType.CHARACTER
        '"funcky:types".unify "funcky:types".Character $_'                                                                                                                                 || FunckySimpleType.CHARACTER
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                     || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.NUMBER)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function "funcky:types".Type $_)'                                        || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.NUMBER)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number)'                                      || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.NUMBER)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function $_ $_)'                                                         || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.NUMBER)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) $_'                                                                                      || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.NUMBER)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                        || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.NUMBER)
        '"funcky:types".domain ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type $_))'                                   || FunckySimpleType.TYPE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type $_)))'      || FunckyBoolean.TRUE
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ "funcky:types".Number)'                                                         || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.NUMBER)
        '"funcky:types".domain ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ $_))'                                                    || FunckySimpleType.TYPE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ $_)))'                       || FunckyBoolean.TRUE
        '"funcky:types".domain ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) $_)'                                                                                 || FunckySimpleType.TYPE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) $_))'                                                    || FunckyBoolean.TRUE
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function "funcky:types".Type $_)'                                                         || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.NUMBER)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number)))' || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number))'                                || FunckySimpleType.NUMBER
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ $_)))'                    || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ $_))'                                                   || FunckySimpleType.NUMBER
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) $_))'                                                 || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) $_)'                                                                                || FunckySimpleType.NUMBER
        '"funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function "funcky:types".Type $_))'                                                    || FunckySimpleType.TYPE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function "funcky:types".Type $_)))'                       || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function $_ "funcky:types".Number)))'                    || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function $_ "funcky:types".Number))'                                                   || FunckySimpleType.NUMBER
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function $_ $_)))'                                       || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function $_ $_)))'                                        || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ $_) $_))'                                                                    || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ $_) $_))'                                                                     || FunckyBoolean.TRUE
        '"funcky:types".unify ("funcky:types".List "funcky:types".Type) ("funcky:types".List "funcky:types".Type)'                                                                         || new FunckyListType(FunckySimpleType.TYPE)
        '"funcky:types".unify ("funcky:types".List "funcky:types".Type) ("funcky:types".List $_)'                                                                                          || new FunckyListType(FunckySimpleType.TYPE)
        '"funcky:types".unify ("funcky:types".List "funcky:types".Type) $_'                                                                                                                || new FunckyListType(FunckySimpleType.TYPE)
        '"funcky:types".unify ("funcky:types".List $_) ("funcky:types".List "funcky:types".Type)'                                                                                          || new FunckyListType(FunckySimpleType.TYPE)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".unify ("funcky:types".List $_) ("funcky:types".List $_)))'                                                    || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".unify ("funcky:types".List $_) $_))'                                                                          || FunckyBoolean.TRUE
        '"funcky:types".unify ("funcky:types".List "funcky:types".Character) "funcky:types".String'                                                                                        || FunckyListType.STRING
        '"funcky:types".unify "funcky:types".String ("funcky:types".List "funcky:types".Character)'                                                                                        || FunckyListType.STRING
        '"funcky:types".unify "funcky:types".String "funcky:types".String'                                                                                                                 || FunckyListType.STRING
        '"funcky:types".unify "funcky:types".String $_'                                                                                                                                    || FunckyListType.STRING
        '"funcky:types".unify ("funcky:types".Record []) ("funcky:types".Record [])'                                                                                                       || FunckyRecordType.UNIT
        '"funcky:types".unify ("funcky:types".Record []) "funcky:types".Unit'                                                                                                              || FunckyRecordType.UNIT
        '"funcky:types".unify ("funcky:types".Record []) $_'                                                                                                                               || FunckyRecordType.UNIT
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record ["funcky:types".Type])'                                                                 || new FunckyRecordType(engine.converter.convert([FunckySimpleType.TYPE]))
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record [$_])'                                                                                  || new FunckyRecordType(engine.converter.convert([FunckySimpleType.TYPE]))
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) $_'                                                                                                            || new FunckyRecordType(engine.converter.convert([FunckySimpleType.TYPE]))
        '"funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".Record ["funcky:types".Type])'                                                                                  || new FunckyRecordType(engine.converter.convert([FunckySimpleType.TYPE]))
        '"funcky:types".typeVariable ("funcky:lists".head ("funcky:types".components ("funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".Record [$_]))))'                   || FunckyBoolean.TRUE
        '"funcky:lists".empty ("funcky:lists".tail ("funcky:types".components ("funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".Record [$_]))))'                          || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:lists".head ("funcky:types".components ("funcky:types".unify ("funcky:types".Record [$_]) $_)))'                                             || FunckyBoolean.TRUE
        '"funcky:lists".empty ("funcky:lists".tail ("funcky:types".components ("funcky:types".unify ("funcky:types".Record [$_]) $_)))'                                                    || FunckyBoolean.TRUE
        '"funcky:types".unify "funcky:types".Unit ("funcky:types".Record [])'                                                                                                              || FunckyRecordType.UNIT
        '"funcky:types".unify "funcky:types".Unit "funcky:types".Unit'                                                                                                                     || FunckyRecordType.UNIT
        '"funcky:types".unify "funcky:types".Unit $_'                                                                                                                                      || FunckyRecordType.UNIT
        '"funcky:types".unify $_ "funcky:types".Type'                                                                                                                                      || FunckySimpleType.TYPE
        '"funcky:types".unify $_ "funcky:types".Number'                                                                                                                                    || FunckySimpleType.NUMBER
        '"funcky:types".unify $_ "funcky:types".Boolean'                                                                                                                                   || FunckySimpleType.BOOLEAN
        '"funcky:types".unify $_ "funcky:types".Character'                                                                                                                                 || FunckySimpleType.CHARACTER
        '"funcky:types".unify $_ ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                                      || new FunckyFunctionType(FunckySimpleType.TYPE, FunckySimpleType.NUMBER)
        '"funcky:types".domain ("funcky:types".unify $_ ("funcky:types".Function "funcky:types".Type $_))'                                                                                 || FunckySimpleType.TYPE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify $_ ("funcky:types".Function "funcky:types".Type $_)))'                                                    || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify $_ ("funcky:types".Function $_ "funcky:types".Type)))'                                                   || FunckyBoolean.TRUE
        '"funcky:types".range ("funcky:types".unify $_ ("funcky:types".Function $_ "funcky:types".Type))'                                                                                  || FunckySimpleType.TYPE
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify $_ ("funcky:types".Function $_ $_)))'                                                                    || FunckyBoolean.TRUE
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify $_ ("funcky:types".Function $_ $_)))'                                                                     || FunckyBoolean.TRUE
        '"funcky:types".unify $_ ("funcky:types".List "funcky:types".Type)'                                                                                                                || new FunckyListType(FunckySimpleType.TYPE)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".unify $_ ("funcky:types".List $_)))'                                                                          || FunckyBoolean.TRUE
        '"funcky:types".unify $_ "funcky:types".String'                                                                                                                                    || FunckyListType.STRING
        '"funcky:types".unify $_ ("funcky:types".Record [])'                                                                                                                               || FunckyRecordType.UNIT
        '"funcky:types".unify $_ ("funcky:types".Record ["funcky:types".Type])'                                                                                                            || new FunckyRecordType(engine.converter.convert([FunckySimpleType.TYPE]))
        '"funcky:types".typeVariable ("funcky:lists".head ("funcky:types".components ("funcky:types".unify $_ ("funcky:types".Record [$_]))))'                                             || FunckyBoolean.TRUE
        '"funcky:lists".empty ("funcky:lists".tail ("funcky:types".components ("funcky:types".unify $_ ("funcky:types".Record [$_]))))'                                                    || FunckyBoolean.TRUE
        '"funcky:types".unify $_ "funcky:types".Unit'                                                                                                                                      || FunckyRecordType.UNIT
        '"funcky:types".typeVariable ("funcky:types".unify $_ $_)'                                                                                                                         || FunckyBoolean.TRUE
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
