package io.github.thanospapapetrou.funcky.prelude

import java.util.regex.Pattern

import io.github.thanospapapetrou.funcky.BaseSpec
import io.github.thanospapapetrou.funcky.runtime.FunckyList
import io.github.thanospapapetrou.funcky.runtime.FunckyValue
import io.github.thanospapapetrou.funcky.runtime.exceptions.FunckyRuntimeException
import io.github.thanospapapetrou.funcky.runtime.prelude.Types
import io.github.thanospapapetrou.funcky.runtime.FunckyFunctionType
import io.github.thanospapapetrou.funcky.runtime.FunckyListType
import io.github.thanospapapetrou.funcky.runtime.FunckyRecordType
import spock.lang.Unroll

class TypesSpec extends BaseSpec {
    @Unroll('Test simple types (expression: #expression)')
    def 'Test simple types'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                     || result
        '"funcky:types".Type'                          || $Type
        '"funcky:types".type "funcky:types".Type'      || $Type
        '"funcky:types".Number'                        || $Number
        '"funcky:types".type "funcky:types".Number'    || $Type
        '"funcky:types".Boolean'                       || $Boolean
        '"funcky:types".type "funcky:types".Boolean'   || $Type
        '"funcky:types".Character'                     || $Character
        '"funcky:types".type "funcky:types".Character' || $Type
    }

    @Unroll('Test Function (expression: #expression)')
    def 'Test Function'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                             || result
        '"funcky:types".Function'                                                                              || new Types(engine).$Function
        '"funcky:types".type "funcky:types".Function'                                                          || new FunckyFunctionType(engine, $Type, $Type, $Type)
        '"funcky:types".type ("funcky:types".Function "funcky:types".Type)'                                    || new FunckyFunctionType(engine, $Type, $Type)
        '"funcky:commons".string ("funcky:types".Function ("funcky:commons".error "foo"))'                     || engine.converter.convert('"funcky:types".Function ("funcky:commons".error "foo")')
        '"funcky:types".Function "funcky:types".Type "funcky:types".Number'                                    || new FunckyFunctionType(engine, $Type, $Number)
        '"funcky:commons".string ("funcky:types".Function "funcky:types".Type ("funcky:commons".error "foo"))' || engine.converter.convert('"funcky:types".Function "funcky:types".Type ("funcky:commons".error "foo")')
    }

    @Unroll('Test domain (expression: #expression)')
    def 'Test domain'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                  || result
        '"funcky:types".domain'                                                                     || new Types(engine).$domain
        '"funcky:types".type "funcky:types".domain'                                                 || new FunckyFunctionType(engine, $Type, $Type)
        '"funcky:types".domain ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || $Type
    }

    def 'Test domain (runtime error)'() {
        when:
        engine.eval('"funcky:types".domain "funcky:types".Type')
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(String.format(Types.ERROR_DOMAIN, $Type))
    }

    @Unroll('Test range (expression: #expression)')
    def 'Test range'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                 || result
        '"funcky:types".range'                                                                     || new Types(engine).$range
        '"funcky:types".type "funcky:types".range'                                                 || new FunckyFunctionType(engine, $Type, $Type)
        '"funcky:types".range ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || $Number
    }

    def 'Test range (runtime error)'() {
        when:
        engine.eval('"funcky:types".range "funcky:types".Type')
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(String.format(Types.ERROR_RANGE, $Type))
    }

    @Unroll('Test List (expression: #expression)')
    def 'Test List'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                     || result
        '"funcky:types".List'                                                          || new Types(engine).$List
        '"funcky:types".type "funcky:types".List'                                      || new FunckyFunctionType(engine, $Type, $Type)
        '"funcky:types".List "funcky:types".Type'                                      || new FunckyListType(engine, $Type)
        '"funcky:commons".string ("funcky:types".List ("funcky:commons".error "foo"))' || engine.converter.convert('"funcky:types".List ("funcky:commons".error "foo")')
    }

    @Unroll('Test element (expression: #expression)')
    def 'Test element'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                         || result
        '"funcky:types".element'                                           || new Types(engine).$element
        '"funcky:types".type "funcky:types".element'                       || new FunckyFunctionType(engine, $Type, $Type)
        '"funcky:types".element ("funcky:types".List "funcky:types".Type)' || $Type
    }

    def 'Test element (runtime error)'() {
        when:
        engine.eval('"funcky:types".element "funcky:types".Type')
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(String.format(Types.ERROR_ELEMENT, $Type))
    }

    @Unroll('Test String (expression: #expression)')
    def 'Test String'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                  || result
        '"funcky:types".String'                     || $String
        '"funcky:types".type "funcky:types".String' || $Type

    }

    @Unroll('Test Record (expression: #expression)')
    def 'Test Record'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                    || result
        '"funcky:types".Record'                       || new Types(engine).$Record
        '"funcky:types".type "funcky:types".Record'   || new FunckyFunctionType(engine, new FunckyListType(engine, $Type), $Type)
        '"funcky:types".Record []'                    || new FunckyRecordType(engine, new FunckyList(engine, new FunckyListType(engine, $Type), null, null))
        '"funcky:types".Record ["funcky:types".Type]' || new FunckyRecordType(engine, new FunckyList(engine, new FunckyListType(engine, $Type), $Type, new FunckyList(engine, new FunckyListType(engine, new FunckyListType(engine, $Type)), null, null)))
    }

    @Unroll('Test components (expression: #expression)')
    def 'Test components'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                || result
        '"funcky:types".components'                                               || new Types(engine).$components
        '"funcky:types".type "funcky:types".components'                           || new FunckyFunctionType(engine, $Type, new FunckyListType(engine, $Type))
        '"funcky:types".components ("funcky:types".Record [])'                    || engine.converter.convert([])
        '"funcky:types".components ("funcky:types".Record ["funcky:types".Type])' || engine.converter.convert([$Type])
    }

    def 'Test components (runtime error)'() {
        when:
        engine.eval('"funcky:types".components "funcky:types".Type')
        then:
        final FunckyRuntimeException e = thrown()
        e.message
        e.message.startsWith(String.format(Types.ERROR_COMPONENTS, $Type))
    }

    @Unroll('Test Unit (expression: #expression)')
    def 'Test Unit'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                || result
        '"funcky:types".Unit'                     || new FunckyRecordType(engine, engine.converter.convert(([])))
        '"funcky:types".type "funcky:types".Unit' || $Type
    }

    @Unroll('Test type (expression: #expression)')
    def 'Test type'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                      || result
        '"funcky:types".type'                                                                           || new Types(engine).$type
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".type "funcky:types".type))' || $true
        '"funcky:types".type "funcky:types".Type'                                                       || $Type
        '"funcky:types".type 1'                                                                         || $Number
        '"funcky:types".type "funcky:booleans".false'                                                   || $Boolean
        '"funcky:types".type \'a\''                                                                     || $Character
        '"funcky:types".type "funcky:types".free'                                                       || new FunckyFunctionType(engine, $Type, $Type)
        '"funcky:types".listType ("funcky:types".type [])'                                              || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".type []))'                 || $true
        '"funcky:types".type [1]'                                                                       || new FunckyListType(engine, $Number)
        '"funcky:types".type ""'                                                                        || $String
        '"funcky:types".type {}'                                                                        || $Unit
        '"funcky:types".type {1}'                                                                       || new FunckyRecordType(engine, engine.converter.convert([$Number]))
        '"funcky:types".type {1, \'a\'}'                                                                || new FunckyRecordType(engine, engine.converter.convert([$Number, $Character]))
    }

    @Unroll('Test typeVariable (expression: #expression)')
    def 'Test typeVariable'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                        || result
        '"funcky:types".typeVariable'                                                                     || new Types(engine).$typeVariable
        '"funcky:types".type "funcky:types".typeVariable'                                                 || new FunckyFunctionType(engine, $Type, $Boolean)
        '"funcky:types".typeVariable "funcky:types".Type'                                                 || $false
        '"funcky:types".typeVariable "funcky:types".Number'                                               || $false
        '"funcky:types".typeVariable "funcky:types".Boolean'                                              || $false
        '"funcky:types".typeVariable "funcky:types".Character'                                            || $false
        '"funcky:types".typeVariable ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || $false
        '"funcky:types".typeVariable ("funcky:types".List "funcky:types".Type)'                           || $false
        '"funcky:types".typeVariable ("funcky:types".String)'                                             || $false
        '"funcky:types".typeVariable ("funcky:types".Record [])'                                          || $false
        '"funcky:types".typeVariable ("funcky:types".Record ["funcky:types".Type])'                       || $false
        '"funcky:types".typeVariable "funcky:types".Unit'                                                 || $false
        '"funcky:types".typeVariable $_'                                                                  || $true
    }

    @Unroll('Test functionType (expression: #expression)')
    def 'Test functionType'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                        || result
        '"funcky:types".functionType'                                                                     || new Types(engine).$functionType
        '"funcky:types".type "funcky:types".functionType'                                                 || new FunckyFunctionType(engine, $Type, $Boolean)
        '"funcky:types".functionType "funcky:types".Type'                                                 || $false
        '"funcky:types".functionType "funcky:types".Number'                                               || $false
        '"funcky:types".functionType "funcky:types".Boolean'                                              || $false
        '"funcky:types".functionType "funcky:types".Character'                                            || $false
        '"funcky:types".functionType ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || $true
        '"funcky:types".functionType ("funcky:types".List "funcky:types".Type)'                           || $false
        '"funcky:types".functionType ("funcky:types".String)'                                             || $false
        '"funcky:types".functionType ("funcky:types".Record [])'                                          || $false
        '"funcky:types".functionType ("funcky:types".Record ["funcky:types".Type])'                       || $false
        '"funcky:types".functionType "funcky:types".Unit'                                                 || $false
        '"funcky:types".functionType $_'                                                                  || $false
    }

    @Unroll('Test listType (expression: #expression)')
    def 'Test listType'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                    || result
        '"funcky:types".listType'                                                                     || new Types(engine).$listType
        '"funcky:types".type "funcky:types".listType'                                                 || new FunckyFunctionType(engine, $Type, $Boolean)
        '"funcky:types".listType "funcky:types".Type'                                                 || $false
        '"funcky:types".listType "funcky:types".Number'                                               || $false
        '"funcky:types".listType "funcky:types".Boolean'                                              || $false
        '"funcky:types".listType "funcky:types".Character'                                            || $false
        '"funcky:types".listType ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || $false
        '"funcky:types".listType ("funcky:types".List "funcky:types".Type)'                           || $true
        '"funcky:types".listType ("funcky:types".String)'                                             || $true
        '"funcky:types".listType ("funcky:types".Record [])'                                          || $false
        '"funcky:types".listType ("funcky:types".Record ["funcky:types".Type])'                       || $false
        '"funcky:types".listType "funcky:types".Unit'                                                 || $false
        '"funcky:types".listType $_'                                                                  || $false
    }

    @Unroll('Test recordType (expression: #expression)')
    def 'Test recordType'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                      || result
        '"funcky:types".recordType'                                                                     || new Types(engine).$recordType
        '"funcky:types".type "funcky:types".recordType'                                                 || new FunckyFunctionType(engine, $Type, $Boolean)
        '"funcky:types".recordType "funcky:types".Type'                                                 || $false
        '"funcky:types".recordType "funcky:types".Number'                                               || $false
        '"funcky:types".recordType "funcky:types".Boolean'                                              || $false
        '"funcky:types".recordType "funcky:types".Character'                                            || $false
        '"funcky:types".recordType ("funcky:types".Function "funcky:types".Type "funcky:types".Number)' || $false
        '"funcky:types".recordType ("funcky:types".List "funcky:types".Type)'                           || $false
        '"funcky:types".recordType ("funcky:types".String)'                                             || $false
        '"funcky:types".recordType ("funcky:types".Record [])'                                          || $true
        '"funcky:types".recordType ("funcky:types".Record ["funcky:types".Type])'                       || $true
        '"funcky:types".recordType "funcky:types".Unit'                                                 || $true
        '"funcky:types".recordType $_'                                                                  || $false
    }

    @Unroll('Test free (expression: #expression')
    def 'Test free'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                                       || result
        '"funcky:types".free'                                                                                                                                                                            || new Types(engine).$free
        '"funcky:types".type "funcky:types".free'                                                                                                                                                        || new FunckyFunctionType(engine, $Type, $Type)
        '"funcky:types".free "funcky:types".Type'                                                                                                                                                        || $Type
        '"funcky:types".free "funcky:types".Number'                                                                                                                                                      || $Number
        '"funcky:types".free "funcky:types".Boolean'                                                                                                                                                     || $Boolean
        '"funcky:types".free "funcky:types".Character'                                                                                                                                                   || $Character
        '"funcky:types".free ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                                                        || new FunckyFunctionType(engine, $Type, $Number)
        '"funcky:types".domain ("funcky:types".free ("funcky:types".Function "funcky:types".Type $_))'                                                                                                   || $Type
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".free ("funcky:types".Function "funcky:types".Type $_)))'                                                                      || $true
        '"funcky:commons".equal ("funcky:types".range ("funcky:types".free ("funcky:types".Function "funcky:types".Type $_))) ("funcky:types".range ("funcky:types".Function "funcky:types".Type $_))'   || $false
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".free ("funcky:types".Function $_ "funcky:types".Type)))'                                                                     || $true
        '"funcky:commons".equal ("funcky:types".domain ("funcky:types".free ("funcky:types".Function $_ "funcky:types".Type))) ("funcky:types".domain ("funcky:types".Function $_ "funcky:types".Type))' || $false
        '"funcky:types".range ("funcky:types".free ("funcky:types".Function $_ "funcky:types".Type))'                                                                                                    || $Type
        '"funcky:types".free ("funcky:types".List "funcky:types".Type)'                                                                                                                                  || new FunckyListType(engine, $Type)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".free ("funcky:types".List $_)))'                                                                                            || $true
        '"funcky:commons".equal ("funcky:types".element ("funcky:types".free ("funcky:types".List $_))) ("funcky:types".element ("funcky:types".List $_))'                                               || $false
        '"funcky:types".free "funcky:types".String'                                                                                                                                                      || $String
        '"funcky:types".free ("funcky:types".Record [])'                                                                                                                                                 || $Unit
        '"funcky:types".free ("funcky:types".Record ["funcky:types".Type])'                                                                                                                              || new FunckyRecordType(engine, engine.converter.convert([$Type]))
        '"funcky:types".typeVariable ("funcky:lists".head ("funcky:types".components ("funcky:types".free ("funcky:types".Record [$_]))))'                                                               || $true
        '"funcky:lists".empty ("funcky:lists".tail ("funcky:types".components ("funcky:types".free ("funcky:types".Record [$_]))))'                                                                      || $true
        '"funcky:types".free "funcky:types".Unit'                                                                                                                                                        || $Unit
        '"funcky:types".typeVariable ("funcky:types".free $_)'                                                                                                                                           || $true
        '"funcky:commons".equal ("funcky:types".free $_) $_'                                                                                                                                             || $false
    }

    @Unroll('Test unify (expression: #expression')
    def 'Test unify'(final String expression, final FunckyValue result) {
        expect:
        engine.eval(expression) == result
        where:
        expression                                                                                                                                                                         || result
        '"funcky:types".unify'                                                                                                                                                             || new Types(engine).$unify
        '"funcky:types".type "funcky:types".unify'                                                                                                                                         || new FunckyFunctionType(engine, $Type, $Type, $Type)
        '"funcky:types".type ("funcky:types".unify "funcky:types".Type)'                                                                                                                   || new FunckyFunctionType(engine, $Type, $Type)
        '"funcky:commons".string ("funcky:types".unify ("funcky:commons".error "foo"))'                                                                                                    || engine.converter.convert('"funcky:types".unify ("funcky:commons".error "foo")')
        '"funcky:types".unify "funcky:types".Type "funcky:types".Type'                                                                                                                     || $Type
        '"funcky:types".unify "funcky:types".Type $_'                                                                                                                                      || $Type
        '"funcky:types".unify "funcky:types".Number "funcky:types".Number'                                                                                                                 || $Number
        '"funcky:types".unify "funcky:types".Number $_'                                                                                                                                    || $Number
        '"funcky:types".unify "funcky:types".Boolean "funcky:types".Boolean'                                                                                                               || $Boolean
        '"funcky:types".unify "funcky:types".Boolean $_'                                                                                                                                   || $Boolean
        '"funcky:types".unify "funcky:types".Character "funcky:types".Character'                                                                                                           || $Character
        '"funcky:types".unify "funcky:types".Character $_'                                                                                                                                 || $Character
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                     || new FunckyFunctionType(engine, $Type, $Number)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function "funcky:types".Type $_)'                                        || new FunckyFunctionType(engine, $Type, $Number)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number)'                                      || new FunckyFunctionType(engine, $Type, $Number)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) ("funcky:types".Function $_ $_)'                                                         || new FunckyFunctionType(engine, $Type, $Number)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type "funcky:types".Number) $_'                                                                                      || new FunckyFunctionType(engine, $Type, $Number)
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                        || new FunckyFunctionType(engine, $Type, $Number)
        '"funcky:types".domain ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type $_))'                                   || $Type
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function "funcky:types".Type $_)))'      || $true
        '"funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ "funcky:types".Number)'                                                         || new FunckyFunctionType(engine, $Type, $Number)
        '"funcky:types".domain ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ $_))'                                                    || $Type
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) ("funcky:types".Function $_ $_)))'                       || $true
        '"funcky:types".domain ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) $_)'                                                                                 || $Type
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function "funcky:types".Type $_) $_))'                                                    || $true
        '"funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function "funcky:types".Type $_)'                                                         || new FunckyFunctionType(engine, $Type, $Number)
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number)))' || $true
        '"funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ "funcky:types".Number))'                                || $Number
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ $_)))'                    || $true
        '"funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) ("funcky:types".Function $_ $_))'                                                   || $Number
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) $_))'                                                 || $true
        '"funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ "funcky:types".Number) $_)'                                                                                || $Number
        '"funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function "funcky:types".Type $_))'                                                    || $Type
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function "funcky:types".Type $_)))'                       || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function $_ "funcky:types".Number)))'                    || $true
        '"funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function $_ "funcky:types".Number))'                                                   || $Number
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function $_ $_)))'                                       || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ $_) ("funcky:types".Function $_ $_)))'                                        || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify ("funcky:types".Function $_ $_) $_))'                                                                    || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify ("funcky:types".Function $_ $_) $_))'                                                                     || $true
        '"funcky:types".unify ("funcky:types".List "funcky:types".Type) ("funcky:types".List "funcky:types".Type)'                                                                         || new FunckyListType(engine, $Type)
        '"funcky:types".unify ("funcky:types".List "funcky:types".Type) ("funcky:types".List $_)'                                                                                          || new FunckyListType(engine, $Type)
        '"funcky:types".unify ("funcky:types".List "funcky:types".Type) $_'                                                                                                                || new FunckyListType(engine, $Type)
        '"funcky:types".unify ("funcky:types".List $_) ("funcky:types".List "funcky:types".Type)'                                                                                          || new FunckyListType(engine, $Type)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".unify ("funcky:types".List $_) ("funcky:types".List $_)))'                                                    || $true
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".unify ("funcky:types".List $_) $_))'                                                                          || $true
        '"funcky:types".unify ("funcky:types".List "funcky:types".Character) "funcky:types".String'                                                                                        || $String
        '"funcky:types".unify "funcky:types".String ("funcky:types".List "funcky:types".Character)'                                                                                        || $String
        '"funcky:types".unify "funcky:types".String "funcky:types".String'                                                                                                                 || $String
        '"funcky:types".unify "funcky:types".String $_'                                                                                                                                    || $String
        '"funcky:types".unify ("funcky:types".Record []) ("funcky:types".Record [])'                                                                                                       || $Unit
        '"funcky:types".unify ("funcky:types".Record []) "funcky:types".Unit'                                                                                                              || $Unit
        '"funcky:types".unify ("funcky:types".Record []) $_'                                                                                                                               || $Unit
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record ["funcky:types".Type])'                                                                 || new FunckyRecordType(engine, engine.converter.convert([$Type]))
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) ("funcky:types".Record [$_])'                                                                                  || new FunckyRecordType(engine, engine.converter.convert([$Type]))
        '"funcky:types".unify ("funcky:types".Record ["funcky:types".Type]) $_'                                                                                                            || new FunckyRecordType(engine, engine.converter.convert([$Type]))
        '"funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".Record ["funcky:types".Type])'                                                                                  || new FunckyRecordType(engine, engine.converter.convert([$Type]))
        '"funcky:types".typeVariable ("funcky:lists".head ("funcky:types".components ("funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".Record [$_]))))'                   || $true
        '"funcky:lists".empty ("funcky:lists".tail ("funcky:types".components ("funcky:types".unify ("funcky:types".Record [$_]) ("funcky:types".Record [$_]))))'                          || $true
        '"funcky:types".typeVariable ("funcky:lists".head ("funcky:types".components ("funcky:types".unify ("funcky:types".Record [$_]) $_)))'                                             || $true
        '"funcky:lists".empty ("funcky:lists".tail ("funcky:types".components ("funcky:types".unify ("funcky:types".Record [$_]) $_)))'                                                    || $true
        '"funcky:types".unify "funcky:types".Unit ("funcky:types".Record [])'                                                                                                              || $Unit
        '"funcky:types".unify "funcky:types".Unit "funcky:types".Unit'                                                                                                                     || $Unit
        '"funcky:types".unify "funcky:types".Unit $_'                                                                                                                                      || $Unit
        '"funcky:types".unify $_ "funcky:types".Type'                                                                                                                                      || $Type
        '"funcky:types".unify $_ "funcky:types".Number'                                                                                                                                    || $Number
        '"funcky:types".unify $_ "funcky:types".Boolean'                                                                                                                                   || $Boolean
        '"funcky:types".unify $_ "funcky:types".Character'                                                                                                                                 || $Character
        '"funcky:types".unify $_ ("funcky:types".Function "funcky:types".Type "funcky:types".Number)'                                                                                      || new FunckyFunctionType(engine, $Type, $Number)
        '"funcky:types".domain ("funcky:types".unify $_ ("funcky:types".Function "funcky:types".Type $_))'                                                                                 || $Type
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify $_ ("funcky:types".Function "funcky:types".Type $_)))'                                                    || $true
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify $_ ("funcky:types".Function $_ "funcky:types".Type)))'                                                   || $true
        '"funcky:types".range ("funcky:types".unify $_ ("funcky:types".Function $_ "funcky:types".Type))'                                                                                  || $Type
        '"funcky:types".typeVariable ("funcky:types".domain ("funcky:types".unify $_ ("funcky:types".Function $_ $_)))'                                                                    || $true
        '"funcky:types".typeVariable ("funcky:types".range ("funcky:types".unify $_ ("funcky:types".Function $_ $_)))'                                                                     || $true
        '"funcky:types".unify $_ ("funcky:types".List "funcky:types".Type)'                                                                                                                || new FunckyListType(engine, $Type)
        '"funcky:types".typeVariable ("funcky:types".element ("funcky:types".unify $_ ("funcky:types".List $_)))'                                                                          || $true
        '"funcky:types".unify $_ "funcky:types".String'                                                                                                                                    || $String
        '"funcky:types".unify $_ ("funcky:types".Record [])'                                                                                                                               || $Unit
        '"funcky:types".unify $_ ("funcky:types".Record ["funcky:types".Type])'                                                                                                            || new FunckyRecordType(engine, engine.converter.convert([$Type]))
        '"funcky:types".typeVariable ("funcky:lists".head ("funcky:types".components ("funcky:types".unify $_ ("funcky:types".Record [$_]))))'                                             || $true
        '"funcky:lists".empty ("funcky:lists".tail ("funcky:types".components ("funcky:types".unify $_ ("funcky:types".Record [$_]))))'                                                    || $true
        '"funcky:types".unify $_ "funcky:types".Unit'                                                                                                                                      || $Unit
        '"funcky:types".typeVariable ("funcky:types".unify $_ $_)'                                                                                                                         || $true
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
