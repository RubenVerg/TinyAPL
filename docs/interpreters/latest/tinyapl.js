// @ts-self-types="./tinyapl.d.ts"
// @ts-ignore Import from web not supported
import { WASI, OpenFile, File, ConsoleStdout } from 'https://unpkg.com/@bjorn3/browser_wasi_shim@0.4.1/dist/index.js';
import ghc_wasm_jsffi from './ghc_wasm_jsffi.js';
const files = [
    new OpenFile(new File([], {})), // stdin
    ConsoleStdout.lineBuffered((msg) => console.log(`[WASI] ${msg}`)), // stdout
    ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI] ${msg}`)), // stderr
];
const options = {};
const wasi = new WASI([], [], files, options);
const instanceExports = {};
const url = 'resolve' in import.meta ? import.meta.resolve('./tinyapl-js.wasm') : './tinyapl-js.wasm';
const { instance } = await WebAssembly.instantiateStreaming(fetch(url), {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghc_wasm_jsffi(instanceExports),
});
Object.assign(instanceExports, instance.exports);
wasi.initialize(instance);
const exports = instance.exports;
await exports.hs_start();
/**
 * Create a new context for TinyAPL code
 * @param input Function providing standard input
 * @param output Function providing standard output
 * @param error Function providing standard error
 * @param quads Quad names available to the interpreter
 */
export async function newContext(input, output, error, quads, directory) {
    return await exports.tinyapl_newContext(input, output, error, quads, directory);
}
/**
 * Run code in a context
 * @param context Context ID
 * @returns A pair containing the result of the code or the error and whether running succeeded
 */
export async function runCode(context, code) {
    return await exports.tinyapl_runCode(context, code);
}
/**
 * List of all global names
 * @param context Context ID
 */
export async function getGlobals(context) {
    return await exports.tinyapl_getGlobals(context);
}
/**
 * Access a global by name
 * @param context Context ID
 */
export async function getGlobal(context, name) {
    return await exports.tinyapl_getGlobal(context, name);
}
/**
 * Set a global by name
 * @param context Context ID
 */
export async function setGlobal(context, name, val) {
    return await exports.tinyapl_setGlobal(context, name, val);
}
/**
 * Higlight a piece of code
 */
export async function highlight(code) {
    return await exports.tinyapl_highlight(code);
}
/**
 * Split a string into UTF32 codepoints
 */
export async function splitString(str) {
    return await exports.tinyapl_splitString(str);
}
/**
 * Join a string of UTF32 codepoints
 */
export async function joinString(strs) {
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
export const colors = Object.fromEntries(await Promise.all(Object.entries(instance.exports).filter(([k]) => k.startsWith('tinyapl_hl')).map(async ([k, v]) => [k['tinyapl_hl'.length].toLowerCase() + k.slice('tinyapl_hl'.length + 1), await v()])));
export const colorsInv = Object.fromEntries(Object.entries(colors).map(([k, v]) => [v, k]));
export const errors = Object.fromEntries(await Promise.all(Object.entries(instance.exports).filter(([k]) => k.startsWith('tinyapl_err')).map(async ([k, v]) => [k['tinyapl_err'.length].toLowerCase() + k.slice('tinyapl_err'.length + 1), await v()])));
/**
 * Turn a `Value` into a string
 */
export async function show(o) {
    return await exports.tinyapl_show(o);
}
/**
 * Turn a `Value` into a string that is more likely to be parseable again
 */
export async function repr(o) {
    return await exports.tinyapl_repr(o);
}
/**
 * Arrow corresponding to a variable type
 */
export async function varArrow(varType) {
    return await exports.tinyapl_varArrow(varType);
}
