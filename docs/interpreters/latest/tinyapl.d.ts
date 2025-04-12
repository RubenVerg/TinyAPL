declare global {
    interface ImportMeta {
        resolve(specifier: string): string;
    }
}
export type Complex = [number, number];
export type VariableType = 'normal' | 'constant' | 'private';
export type StructEntry = [VariableType, Value];
export interface Struct {
    type: 'struct';
    entries: Record<string, StructEntry>;
}
export type ScalarValue = Complex | string | Noun | Fun | Adv | Conj | Struct;
export type DictEntry = [ScalarValue, ScalarValue];
export type ExtraArgs = DictEntry[];
export interface Arr {
    type: 'array';
    shape: number[];
    contents: ScalarValue[];
}
export interface Dict {
    type: 'dictionary';
    entries: DictEntry[];
}
export type Noun = Arr | Dict;
export interface Nilad {
    type: 'nilad';
    repr: string;
    get?: () => PromiseLike<Err | Noun>;
    set?: (arr: Noun) => PromiseLike<Err | void>;
}
export interface Fun {
    type: 'function';
    repr: string;
    monad?: (ea: ExtraArgs, y: Noun) => PromiseLike<Err | Noun>;
    dyad?: (ea: ExtraArgs, x: Noun, y: Noun) => PromiseLike<Err | Noun>;
}
export interface Adv {
    type: 'adverb';
    repr: string;
    array?: (ea: ExtraArgs, n: Noun) => PromiseLike<Err | Fun>;
    function?: (ea: ExtraArgs, f: Fun) => PromiseLike<Err | Fun>;
}
export interface Conj {
    type: 'conjunction';
    repr: string;
    arrayArray?: (ea: ExtraArgs, n: Noun, m: Noun) => PromiseLike<Err | Fun>;
    arrayFunction?: (ea: ExtraArgs, n: Noun, f: Fun) => PromiseLike<Err | Fun>;
    functionArray?: (ea: ExtraArgs, f: Fun, m: Noun) => PromiseLike<Err | Fun>;
    functionFunction?: (ea: ExtraArgs, f: Fun, g: Fun) => PromiseLike<Err | Fun>;
}
export type Value = Noun | Fun | Adv | Conj;
export interface Err {
    code: number;
    message: string;
}
export type Quads = Record<string, Nilad | Fun | Adv | Conj>;
/**
 * Create a new context for TinyAPL code
 * @param input Function providing standard input
 * @param output Function providing standard output
 * @param error Function providing standard error
 * @param quads Quad names available to the interpreter
 */
export declare function newContext(input: () => PromiseLike<string>, output: (what: string) => PromiseLike<void>, error: (what: string) => PromiseLike<void>, quads: Quads, directory: string): Promise<number>;
/**
 * Run code in a context
 * @param context Context ID
 * @returns A pair containing the result of the code or the error and whether running succeeded
 */
export declare function runCode(context: number, code: string): Promise<Err | Value>;
/**
 * List of all global names
 * @param context Context ID
 */
export declare function getGlobals(context: number): Promise<string[]>;
/**
 * Access a global by name
 * @param context Context ID
 */
export declare function getGlobal(context: number, name: string): Promise<Err | Value>;
/**
 * Set a global by name
 * @param context Context ID
 */
export declare function setGlobal(context: number, name: string, val: Value): Promise<Err | void>;
/**
 * Higlight a piece of code
 */
export declare function highlight(code: string): Promise<number[]>;
/**
 * Split a string into UTF32 codepoints
 */
export declare function splitString(str: string): Promise<string[]>;
/**
 * Join a string of UTF32 codepoints
 */
export declare function joinString(strs: string[]): Promise<string>;
export declare const glyphs: {
    syntax: any;
    identifiers: any;
    arrays: any;
    functions: any;
    adverbs: any;
    conjunctions: any;
};
export declare const colors: Record<string, number>;
export declare const colorsInv: Record<number, string>;
export declare const errors: Record<string, number>;
/**
 * Turn a `Value` into a string
 */
export declare function show(o: Err | Value): Promise<string>;
/**
 * Turn a `Value` into a string that is more likely to be parseable again
 */
export declare function repr(o: Value): Promise<string>;
/**
 * Arrow corresponding to a variable type
 */
export declare function varArrow(varType: VariableType): Promise<string>;
