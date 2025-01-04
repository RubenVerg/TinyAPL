// @ts-self-types="./tinyapl.d.ts"
// @ts-ignore Import from web not supported
import { WASI, OpenFile, File, ConsoleStdout } from 'https://esm.run/@bjorn3/browser_wasi_shim@0.3.0';
import ghc_wasm_jsffi from './ghc_wasm_jsffi.js';

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

const files = [
	new OpenFile(new File([], {})), // stdin
	ConsoleStdout.lineBuffered((msg: string) => console.log(`[WASI] ${msg}`)), // stdout
	ConsoleStdout.lineBuffered((msg: string) => console.warn(`[WASI] ${msg}`)), // stderr
];
const options = {};
const wasi = new WASI([], [], files, options);

const instanceExports = {};
const url = 'resolve' in import.meta ? import.meta.resolve('./tinyapl-js.wasm') : './tinyapl-js.wasm';
const { instance } = await WebAssembly.instantiateStreaming(fetch(url), {
	wasi_snapshot_preview1: (wasi as any).wasiImport,
	ghc_wasm_jsffi: ghc_wasm_jsffi(instanceExports),
});
Object.assign(instanceExports, instance.exports);

wasi.initialize(instance);

const exports = instance.exports as any;

await exports.hs_start();

/**
 * Create a new context for TinyAPL code
 * @param input Function providing standard input
 * @param output Function providing standard output
 * @param error Function providing standard error
 * @param quads Quad names available to the interpreter
 */
export async function newContext(input: () => PromiseLike<string>, output: (what: string) => PromiseLike<void>, error: (what: string) => PromiseLike<void>, quads: Quads): Promise<number> {
	return await exports.tinyapl_newContext(input, output, error, quads);
}

/**
 * Run code in a context
 * @param context Context ID
 * @returns A pair containing the result of the code or the error and whether running succeeded
 */
export async function runCode(context: number, code: string): Promise<Err | Value> {
	return await exports.tinyapl_runCode(context, code);
}

/**
 * List of all global names
 * @param context Context ID
 */
export async function getGlobals(context: number): Promise<string[]> {
	return await exports.tinyapl_getGlobals(context);
}

/**
 * Access a global by name
 * @param context Context ID
 */
export async function getGlobal(context: number, name: string): Promise<Err | Value> {
	return await exports.tinyapl_getGlobal(context, name);
}

/**
 * Set a global by name
 * @param context Context ID
 */
export async function setGlobal(context: number, name: string, val: Value): Promise<Err | void> {
	return await exports.tinyapl_setGlobal(context, name, val);
}

/**
 * Higlight a piece of code
 */
export async function highlight(code: string): Promise<number[]> {
	return await exports.tinyapl_highlight(code);
}

/**
 * Split a string into UTF32 codepoints
 */
export async function splitString(str: string): Promise<string[]> {
	return await exports.tinyapl_splitString(str);
}

/**
 * Join a string of UTF32 codepoints
 */
export async function joinString(strs: string[]): Promise<string> {
	return await exports.tinyapl_joinString(strs);
}

export const glyphs = {
	syntax: await exports.tinyapl_glyphsSyntax(),
	identifiers: await exports.tinyapl_glyphsIdentifiers(),
	arrays: await exports.tinyapl_glyphsArrays(),
	functions: await exports.tinyapl_glyphsFunctions(),
	adverbs: await exports.tinyapl_glyphsAdverbs(),
	conjunctions: await exports.tinyapl_glyphsConjunctions(),
};

export const colors: Record<string, number> = Object.fromEntries(await Promise.all(Object.entries(instance.exports).filter(([k]) => k.startsWith('tinyapl_hl')).map(async ([k, v]) => [k['tinyapl_hl'.length].toLowerCase() + k.slice('tinyapl_hl'.length + 1), await (v as () => Promise<number>)()])));

export const colorsInv: Record<number, string> = Object.fromEntries(Object.entries(colors).map(([k, v]) => [v, k]));

export const errors: Record<string, number> = Object.fromEntries(await Promise.all(Object.entries(instance.exports).filter(([k]) => k.startsWith('tinyapl_err')).map(async ([k, v]) => [k['tinyapl_err'.length].toLowerCase() + k.slice('tinyapl_err'.length + 1), await (v as () => Promise<number>)()])));

/**
 * Turn a `Value` into a string
 */
export async function show(o: Err | Value): Promise<string> {
	return await exports.tinyapl_show(o);
}

/**
 * Turn a `Value` into a string that is more likely to be parseable again
 */
export async function repr(o: Value): Promise<string> {
	return await exports.tinyapl_repr(o);
}

/**
 * Arrow corresponding to a variable type
 */
export async function varArrow(varType: VariableType): Promise<string> {
	return await exports.tinyapl_varArrow(varType);
}
