import * as tinyapl from './tinyapl.js';
import * as wav from './wav.js';

async function callErr<Ret, Args extends unknown[]>(fn: (...args: Args) => PromiseLike<Ret | tinyapl.Err>, ...args: Args): Promise<Ret> {
	const res = await fn(...args);
	if (typeof res === 'object' && res !== null && 'code' in res && 'message' in res) throw res;
	return res;
}

function makeFunction<Args extends unknown[], Ret, ListenerArgs extends unknown[]>(fn: (runListeners: (...args: ListenerArgs) => Promise<void>, ...args: Args) => Ret) {
	let listeners: ((...args: ListenerArgs) => Promise<void>)[] = [];
	const register = (l: (...args: ListenerArgs) => Promise<void>) => { listeners.push(l); };
	const done = () => { listeners = []; };
	const runListeners = async (...args: ListenerArgs) => { for (const l of listeners) await l(...args); }
	return { register, done, fn: (...args: Args) => fn(runListeners, ...args) };
}

function handleEx(ex: unknown): tinyapl.Err {
	if (!(ex instanceof Error) && typeof ex === 'object' && ex !== null && 'code' in ex && 'message' in ex) return ex as tinyapl.Err;
	else if (ex instanceof Error) return { code: tinyapl.errors.user, message: ex.message };
	else return { code: tinyapl.errors.user, message: (ex as any).toString() };
}

function monad(fn: (ea: tinyapl.ExtraArgs, y: tinyapl.Noun) => PromiseLike<tinyapl.Noun>, repr: string): tinyapl.Fun {
	return {
		type: 'function',
		repr,
		monad: async (ea, y) => {
			try {
				return await fn(ea, y);
			} catch (ex) {
				return handleEx(ex);
			}
		},
	};
}

function dyad(fn: (ea: tinyapl.ExtraArgs, x: tinyapl.Noun, y: tinyapl.Noun) => PromiseLike<tinyapl.Noun>, repr: string): tinyapl.Fun {
	return {
		type: 'function',
		repr,
		dyad: async (ea, x, y) => {
			try {
				return await fn(ea, x, y);
			} catch (ex) {
				return handleEx(ex);
			}
		},
	};
}

function ambivalent(m: (ea: tinyapl.ExtraArgs, y: tinyapl.Noun) => PromiseLike<tinyapl.Noun>, d: (ea: tinyapl.ExtraArgs, x: tinyapl.Noun, y: tinyapl.Noun) => PromiseLike<tinyapl.Noun>, repr: string): tinyapl.Fun {
	return {
		type: 'function',
		repr,
		monad: async (ea, x) => {
			try {
				return await m(ea, x)
			} catch (ex) {
				return handleEx(ex);
			}
		},
		dyad: async (ea, x, y) => {
			try {
				return await d(ea, x, y);
			} catch (ex) {
				return handleEx(ex);
			}
		},
	};
}

function ambivalent1(fn: (ea: tinyapl.ExtraArgs, x: tinyapl.Noun, y?: tinyapl.Noun) => PromiseLike<tinyapl.Noun>, repr: string) {
	return ambivalent(fn, fn, repr);
}

function adverbArr(adv: (ea: tinyapl.ExtraArgs, n: tinyapl.Noun) => PromiseLike<tinyapl.Fun>, repr: string): tinyapl.Adv {
	return {
		type: 'adverb',
		repr,
		array: async (ea, n) => {
			try {
				return await adv(ea, n);
			} catch (ex) {
				return handleEx(ex);
			}
		},
	}
}

function adverbFun(adv: (ea: tinyapl.ExtraArgs, f: tinyapl.Fun) => PromiseLike<tinyapl.Fun>, repr: string): tinyapl.Adv {
	return {
		type: 'adverb',
		repr,
		function: async (ea, f) => {
			try {
				return await adv(ea, f);
			} catch (ex) {
				return handleEx(ex);
			}
		},
	}
}

function adverb(arr: (ea: tinyapl.ExtraArgs, n: tinyapl.Noun) => PromiseLike<tinyapl.Fun>, fn: (ea: tinyapl.ExtraArgs, f: tinyapl.Fun) => PromiseLike<tinyapl.Fun>, repr: string): tinyapl.Adv {
	return {
		type: 'adverb',
		repr,
		array: async (ea, n) => {
			try {
				return await arr(ea, n);
			} catch (ex) {
				return handleEx(ex);
			}
		},
		function: async (ea, f) => {
			try {
				return await fn!(ea, f);
			} catch (ex) {
				return handleEx(ex);
			}
		},
	}
}

function adverb1(adv: (ea: tinyapl.ExtraArgs, u: tinyapl.Noun | tinyapl.Fun) => PromiseLike<tinyapl.Fun>, repr: string) {
	return adverb(adv, adv, repr);
}

function makeMonad<ListenerArgs extends unknown[]>(fn: (listener: (...args: ListenerArgs) => PromiseLike<void>, ea: tinyapl.ExtraArgs, y: tinyapl.Noun) => PromiseLike<tinyapl.Noun>, repr: string) {
	const { register, done, fn: fn1 } = makeFunction<[tinyapl.ExtraArgs, tinyapl.Noun], PromiseLike<tinyapl.Noun>, ListenerArgs>(fn);
	return { register, done, fn: monad(fn1, repr) };
}

function makeDyad<ListenerArgs extends unknown[]>(fn: (listener: (...args: ListenerArgs) => PromiseLike<void>, ea: tinyapl.ExtraArgs, x: tinyapl.Noun, y: tinyapl.Noun) => PromiseLike<tinyapl.Noun>, repr: string) {
	const { register, done, fn: fn1 } = makeFunction<[tinyapl.ExtraArgs, tinyapl.Noun, tinyapl.Noun], PromiseLike<tinyapl.Noun>, ListenerArgs>(fn);
	return { register, done, fn: dyad(fn1, repr) };
}

function makeAmbivalent<ListenerArgs extends unknown[]>(m: (listener: (...args: ListenerArgs) => PromiseLike<void>, ea: tinyapl.ExtraArgs, y: tinyapl.Noun) => PromiseLike<tinyapl.Noun>, d: (listener: (...args: ListenerArgs) => PromiseLike<void>, ea: tinyapl.ExtraArgs, x: tinyapl.Noun, y: tinyapl.Noun) => PromiseLike<tinyapl.Noun>, repr: string) {
	let listeners: ((...args: ListenerArgs) => Promise<void>)[] = [];
	const register = (l: (...args: ListenerArgs) => Promise<void>) => { listeners.push(l); };
	const done = () => { listeners = []; };
	const runListeners = async (...args: ListenerArgs) => { for (const l of listeners) await l(...args); }
	return { register, done, fn: ambivalent((ea, y) => m(runListeners, ea, y), (ea, x, y) => d(runListeners, ea, x, y), repr) };
}

function makeAmbivalent1<ListenerArgs extends unknown[]>(fn: (listener: (...args: ListenerArgs) => PromiseLike<void>, ea: tinyapl.ExtraArgs, x: tinyapl.Noun, y?: tinyapl.Noun) => PromiseLike<tinyapl.Noun>, repr: string) {
	return makeAmbivalent(fn, fn, repr);
}

function makeAdverb<ListenerArgs extends unknown[]>(arr: (listener: (...args: ListenerArgs) => PromiseLike<void>, ea: tinyapl.ExtraArgs, n: tinyapl.Noun) => PromiseLike<tinyapl.Fun>, fn: (listener: (...args: ListenerArgs) => PromiseLike<void>, ea: tinyapl.ExtraArgs, f: tinyapl.Fun) => PromiseLike<tinyapl.Fun>, repr: string) {
	let listeners: ((...args: ListenerArgs) => Promise<void>)[] = [];
	const register = (l: (...args: ListenerArgs) => Promise<void>) => { listeners.push(l); };
	const done = () => { listeners = []; };
	const runListeners = async (...args: ListenerArgs) => { for (const l of listeners) await l(...args); }
	return { register, done, fn: adverb((ea, n) => arr(runListeners, ea, n), (ea, f) => fn(runListeners, ea, f), repr) };
}

function makeAdverb1<ListenerArgs extends unknown[]>(arr: (listener: (...args: ListenerArgs) => PromiseLike<void>, ea: tinyapl.ExtraArgs, u: tinyapl.Noun | tinyapl.Fun) => PromiseLike<tinyapl.Fun>, repr: string) {
	return makeAdverb(arr, arr, repr);
}

function toImageData(a: tinyapl.Arr, name: string): ImageData {
	if (a.shape.length !== 2 && a.shape.length !== 3) throw { code: tinyapl.errors.rank, message: `${name} expects arrays of rank 2 or 3` };
	const els = a.shape.length === 2 ? 1 : a.shape.at(-1)!;
	if (![1, 2, 3, 4].includes(els)) throw { code: tinyapl.errors.length, message: `${name}: third axis must have length 1, 2, 3 or 4` };
	const data = new ImageData(a.shape[1], a.shape[0]);
	for (let y = 0; y < a.shape[0]; y++)
		for (let x = 0; x < a.shape[1]; x++) {
			const dIdx = (x * a.shape[0] + y) * 4;
			const uIdx = (x * a.shape[0] + y) * els;
			if (els === 1) {
				data.data[dIdx + 0] = data.data[dIdx + 1] = data.data[dIdx + 2] = (a.contents[uIdx + 0] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 3] = 255;
			} else if (els === 2) {
				data.data[dIdx + 0] = data.data[dIdx + 1] = data.data[dIdx + 2] = (a.contents[uIdx + 0] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 3] = (a.contents[uIdx + 1] as tinyapl.Complex)[0] * 255;
			} else if (els === 3) {
				data.data[dIdx + 0] = (a.contents[uIdx + 0] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 1] = (a.contents[uIdx + 1] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 2] = (a.contents[uIdx + 2] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 3] = 255;
			} else {
				data.data[dIdx + 0] = (a.contents[uIdx + 0] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 1] = (a.contents[uIdx + 1] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 2] = (a.contents[uIdx + 2] as tinyapl.Complex)[0] * 255;
				data.data[dIdx + 3] = (a.contents[uIdx + 3] as tinyapl.Complex)[0] * 255;
			}
		}
	return data;
}

let imageId = 0;

export const { register: rCreateImage, done: dCreateImage, fn: qCreateImage } = makeDyad<[number, number, number]>(async (runListeners, _ea, x, y) => {
	if (y.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕CreateImage expects arrays' };
	if (x.type !== 'array') throw { code: tinyapl.errors.rank, message: '⎕CreateImage expects arrays' };
	if (y.shape.length !== 0 && x.shape.length !== 1) throw { code: tinyapl.errors.rank, message: '⎕CreateImage expects arrays of rank 0 or 1' };
	if (y.shape.length === 1 && x.contents.length !== 2) throw { code: tinyapl.errors.length, message: '⎕CreateImage expects a scalar or a two-element vector' };
	const height = (y.contents[0] as tinyapl.Complex)[0]
	const width = (y.contents[x.shape.length] as tinyapl.Complex)[0];
	const id = ++imageId;
	await runListeners(id, width, height);
	return { type: 'array', shape: [], contents: [[id, 0]] };
}, '⎕CreateImage');

export const { register: rDisplayImage, done: dDisplayImage, fn: qDisplayImage } = makeAmbivalent1<[number | undefined, ImageData]>(async (runListeners, _ea, x, y) => {
	let id, a: tinyapl.Arr;
	if (x.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕DisplayImage expects arrays' };
	if (y) {
		if (y.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕DisplayImage expects arrays' };
		a = y;
		if (x.shape.length !== 0) throw { code: tinyapl.errors.rank, message: '⎕DispayImage left argument must be a scalar natural' };
		id = (x.contents[0] as tinyapl.Complex)[0];
	} else a = x;
	const data = toImageData(a, '⎕DisplayImage');
	await runListeners(id, data);
	return { type: 'array', shape: [0], contents: [] };
}, '⎕DisplayImage');

export const { register: rPlayAnimation, done: dPlayAnimation, fn: qPlayAnimation } = makeAmbivalent1<[number, ImageData[]]>(async (runListeners, _ea, x, y) => {
	let delay, arr;
	if (x.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕PlayAnimation expects arrays' };
	if (y) {
		if (y.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕PlayAnimation expects arrays' };
		arr = y;
		if (x.shape.length !== 0) throw { code: tinyapl.errors.rank, message: '⎕PlayAnimation left argument must be a scalar' };
		delay = (x.contents[0] as tinyapl.Complex)[0];
	} else {
		arr = x;
		delay = 0.1;
	}
	if (arr.shape.length !== 3 && arr.shape.length !== 4) throw { code: tinyapl.errors.rank, message: '⎕PlayAnimation expects arrays of rank 3 or 4' };
	const [frames] = arr.shape;
	const len = arr.shape.slice(1).reduce((a, b) => a * b, 1);
	const datas: ImageData[] = [];
	for (let idx = 0; idx < frames; idx++)
		datas.push(toImageData({ type: 'array', shape: arr.shape.slice(1), contents: arr.contents.slice(idx * len).slice(0, len) }, '⎕PlayAnimation'));
	await runListeners(delay, datas);
	return { type: 'array', shape: [0], contents: [] };
}, '⎕PlayAnimation');

export const { register: rScatterPlot, done: dScatterPlot, fn: qScatterPlot } = makeAmbivalent1<[number[][], number[][], string]>(async (runListeners, _ea, x, y) => {
	let mode = 'markers', arr;
	if (x.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕ScatterPlot expects arrays' };
	if (y) {
		if (y.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕ScatterPlot expects arrays' };
		mode = await tinyapl.joinString(x.contents as string[]);
		arr = y;
	} else arr = x;
	if (arr.shape.length !== 2 && arr.shape.length !== 3) throw { code: tinyapl.errors.rank, message: '⎕ScatterPlot expects arrays of rank 2 or 3' };
	if (arr.shape.at(-1) !== 2) throw { code: tinyapl.errors.length, message: '⎕ScatterPlot argument last axis must be of length 2' };
	const xs = [], ys = [];
	if (arr.shape.length === 2) {
		const x1 = [], y1 = [];
		for (let i = 0; i < 2 * arr.shape[0]; i += 2) {
			x1.push((arr.contents[i] as tinyapl.Complex)[0]);
			y1.push((arr.contents[i + 1] as tinyapl.Complex)[0]);
		}
		xs.push(x1);
		ys.push(y1);
	} else {
		for (let j = 0; j < 2 * arr.shape[1] * arr.shape[0]; j += 2 * arr.shape[1]) {
			const x1 = [], y1 = [];
			for (let i = 0; i < 2 * arr.shape[1]; i += 2) {
				x1.push((arr.contents[j + i] as tinyapl.Complex)[0]);
				y1.push((arr.contents[j + i + 1] as tinyapl.Complex)[0]);
			}
			xs.push(x1);
			ys.push(y1);
		}
	}
	await runListeners(xs, ys, mode);
	return { type: 'array', shape: [0], contents: [] };
}, '⎕ScatterPlot');

export const { register: rGraph, done: dGraph, fn: qGraph } = makeAdverb1<[[number, number][][], string[]]>(async (runListeners, _ea, u) => {
	const graphCounts = 500;
	let fns: tinyapl.Fun[] = [], labels: string[] = [];
	if (u.type === 'function') {
		fns = [u];
		labels = [u.repr];
	} else {
		if (u.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕Graph expects arrays or functions' };
		if (u.shape.length === 0) {
			fns = [u.contents[0] as tinyapl.Fun];
			labels = [fns[0].repr];
		} else if (u.shape.length === 1) {
			for (const c of u.contents) {
				if (typeof c === 'object' && !Array.isArray(c) && c.type === 'function') {
					fns.push(c);
					labels.push(c.repr);
				} else if (typeof c === 'object' && !Array.isArray(c) && c.type === 'array' && c.shape.length === 1 && c.shape[0] === 2) {
					fns.push(c.contents[0] as tinyapl.Fun);
					labels.push(typeof c.contents[1] === 'string' ? c.contents[1] : await tinyapl.joinString((c.contents[1] as tinyapl.Arr).contents as string[]));
				} else throw { code: tinyapl.errors.domain, message: '⎕Graph left operand must be a function or a list of functions or pairs of functions and labels' };
			}
		}
	}
	const graph = async (ea: tinyapl.ExtraArgs, start: number, end: number) => {
		let results: [number, number][][] = [];
		for (const f of fns) {
			results.push([]);			
			for (let n = start; n < end; n += (end - start) / graphCounts) {
				results.at(-1)!.push([n, ((await callErr(f.monad!, ea, { type: 'array', shape: [], contents: [[n, 0]] }) as tinyapl.Arr).contents[0] as tinyapl.Complex)[0]]);
			}
		}
		console.log(results);
		await runListeners(results, labels);
	};
	return ambivalent(async (ea, y) => {
		if (y.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕Graph expects arrays' };
		if (y.shape.length !== 0) throw { code: tinyapl.errors.rank, message: '⎕Graph arguments must be scalar reals' };
		const end = (y.contents[0] as tinyapl.Complex)[0];
		await graph(ea, 0, end);
		return { type: 'array', shape: [0], contents: [] };
	}, async (ea, x, y) => {
		if (x.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕Graph expects arrays' };
		if (y.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕Graph expects arrays' };
		if (x.shape.length !== 0) throw { code: tinyapl.errors.rank, message: '⎕Graph arguments must be scalar reals' };
		if (y.shape.length !== 0) throw { code: tinyapl.errors.rank, message: '⎕Graph arguments must be scalar reals' };
		const start = (x.contents[0] as tinyapl.Complex)[0];
		const end = (y.contents[0] as tinyapl.Complex)[0];
		await graph(ea, start, end);
		return { type: 'array', shape: [0], contents: [] } as tinyapl.Arr;
	}, '(' + await tinyapl.show(u) + ')' + '⎕_Graph');
}, '⎕_Graph');

export const { register: rPlayAudio, done: dPlayAudio, fn: qPlayAudio } = makeAmbivalent1<[ArrayBuffer]>(async (runListeners, _ea, x, y) => {
	let sampleRate, arr;
	if (x.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕PlayAudio expects arrays' };
	if (y) {
		if (x.shape.length !== 0) throw { code: tinyapl.errors.rank, message: '⎕PlayAudio left argument must be scalar' };
		sampleRate = Math.floor((x.contents[0] as tinyapl.Complex)[0]);
		if (y.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕PlayAudio expects arrays' };
		arr = y;
	} else {
		arr = x;
		sampleRate = 44100;
	}
	if (arr.shape.length !== 1 && arr.shape.length !== 2) throw { code: tinyapl.errors.rank, message: '⎕PlayAudio expects arrays of rank 1 or 2' };
	const channels = arr.shape.length === 1 ? 1 : arr.shape[0];
	const length = arr.shape.at(-1)!;
	const bufs = new Array(channels).fill(0).map(_ => new Float32Array(length));
	for (let ch = 0; ch < channels; ch++)
		for (let b = 0; b < length; b++)
			bufs[ch][b] = (arr.contents[ch * length + b] as tinyapl.Complex)[0];
	await runListeners(wav.encode(sampleRate, bufs));
	return { type: 'array', shape: [0], contents: [] };
}, '⎕PlayAudio');

export const qFetch = ambivalent(async (_ea, u) => {
	if (u.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕Fetch expects arrays' };
	if (u.shape.length > 1) throw { code: tinyapl.errors.rank, message: '⎕Fetch expects character vectors' };
	const url = await tinyapl.joinString(u.contents as string[]);
	const text = await fetch(url).then(res => res.text());
	return { type: 'array', shape: [text.length], contents: await tinyapl.splitString(text) };
}, async (_ea, m, u) => {
	if (u.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕Fetch expects arrays' };
	if (m.type !== 'array') throw { code: tinyapl.errors.domain, message: '⎕Fetch expects arrays' };
	if (u.shape.length > 1) throw { code: tinyapl.errors.rank, message: '⎕Fetch expects character vectors' };
	const url = await tinyapl.joinString(u.contents as string[]);
	const typeError = { code: tinyapl.errors.domain, message: '⎕Fetch left argument must be one of ⟨1⋄¯1⋄0ᴊ1⋄0ᴊ¯1⟩×⊞⟨8⋄16⋄32⟩, ⟨1⋄0ᴊ1⟩×⊞⟨0.32⋄0.64⟩ or 1' };
	if (m.shape.length !== 0) throw typeError;
	const mode = (m.contents[0] as tinyapl.Complex).join(';');
	const buf = await fetch(url).then(res => res.arrayBuffer());
	const view = new DataView(buf);
	const result = [];
	switch (mode) {
		case '1;0':
			for (let i = 0; i < view.byteLength; i += 1) {
				const u = view.getUint8(i);
				for (let bi = 0; bi < 8; bi++) {
					result.push(Number((u & (1 << bi)) !== 0));
				}
			}
			break;
		case '8;0':
		case '0;8':
			for (let i = 0; i < view.byteLength; i += 1) result.push(view.getUint8(i));
			break;
		case '-8;0':
		case '0;-8':
			for (let i = 0; i < view.byteLength; i += 1) result.push(view.getInt8(i));
			break;
		case '16;0':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getUint16(i, true));
			break;
		case '-16;0':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getInt16(i, true));
			break;
		case '0;16':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getUint16(i, false));
			break;
		case '0;-16':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getInt16(i, false));
			break;
		case '32;0':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getUint32(i, true));
			break;
		case '-32;0':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getInt32(i, true));
			break;
		case '0;32':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getUint32(i, false));
			break;
		case '0;-32':
			for (let i = 0; i < view.byteLength; i += 2) result.push(view.getInt32(i, false));
			break;
		case '0.32;0':
			for (let i = 0; i < view.byteLength; i += 4) result.push(view.getFloat32(i, true));
			break;
		case '0;0.32':
			for (let i = 0; i < view.byteLength; i += 4) result.push(view.getFloat32(i, false));
			break;
		case '0.64;0':
			for (let i = 0; i < view.byteLength; i += 8) result.push(view.getFloat64(i, true));
			break;
		case '0;0.64':
			for (let i = 0; i < view.byteLength; i += 8) result.push(view.getFloat64(i, false));
			break;
		default:
			throw typeError;
	}
	return { type: 'array', shape: [result.length], contents: result.map(r => [r, 0]) };
}, '⎕Fetch');
