import * as tinyapl from './tinyapl.js';
import * as quads from './quads.js';

import 'https://cdn.plot.ly/plotly-2.34.0.min.js';
// @ts-ignore Import from web not supported
import { encode as _encodeGIF } from 'https://esm.run/modern-gif@2.0.3';

const encodeGIF = _encodeGIF as (_: { width: number, height: number, frames: { data: CanvasImageSource | BufferSource | string; delay: number; }[] }) => Promise<ArrayBuffer>;

import * as _Plotly from 'plotly.js';
declare const Plotly: typeof import('plotly.js');

const buttons = document.querySelector<HTMLDivElement>('#buttons')!;
const output = document.querySelector<HTMLPreElement>('#output')!;
const input = document.querySelector<HTMLTextAreaElement>('#input')!;
const highlighted = document.querySelector<HTMLPreElement>('#highlighted')!;
const button = document.querySelector<HTMLButtonElement>('#button')!;
const infobutton = document.querySelector<HTMLButtonElement>('#infobutton')!;
const info = document.querySelector<HTMLDivElement>('#info')!;
const fancyarrays = document.querySelector<HTMLInputElement>('#fancyarrays')!;

function zip<A, B>(as: A[], bs: B[]): [A, B][] {
	return [...as, ...bs].slice(0, Math.min(as.length, bs.length)).map((_, idx) => [as[idx], bs[idx]]);
}

const prefix = { code: 'Backquote', sym: '`' };

const keyboard = [
	['Backquote', '`', '~', undefined, '⍨', '⋄', '⌺'],
	['Digit1', '1', '!', '¨', '⨳', undefined, '⑴'],
	['Digit2', '2', '@', '¯', undefined, undefined, undefined],
	['Digit3', '3', '#', undefined, '⍒', undefined, undefined],
	['Digit4', '4', '$', '≤', '⍋', '⊴', undefined],
	['Digit5', '5', '%', '⬚', '≈', '⤺', undefined],
	['Digit6', '6', '^', '≥', '⍉', '⊵', undefined],
	['Digit7', '7', '&', undefined, '⊖', undefined, undefined],
	['Digit8', '8', '*', '≠', '⍣', '⍟', '∞'],
	['Digit9', '9', '(', '∨', '⍱', '∻', '⦋'],
	['Digit0', '0', ')', '∧', '⍲', '⍬', '⦌'],
	['Minus', '-', '_', '×', '⊗', '⸚', 'ⵧ'],
	['Equal', '=', '+', '÷', '⊕', '⌹', '⧺'],
	['KeyQ', 'q', 'Q', undefined, undefined, '⇾', '⇽'],
	['KeyW', 'w', 'W', '⍵', '⍹', undefined, undefined],
	['KeyE', 'e', 'E', '∊', '⍷', '⋵', '⋷'],
	['KeyR', 'r', 'R', '⍴', '√', 'ϼ', 'ℜ'],
	['KeyT', 't', 'T', '⊞', '⍨', '߹', '‥'],
	['KeyY', 'y', 'Y', '↑', '↟', 'ᓚ', undefined],
	['KeyU', 'u', 'U', '↓', '↡', 'ᓗ', undefined],
	['KeyI', 'i', 'I', '⍳', '⍸', '…', 'ℑ'],
	['KeyO', 'o', 'O', '○', '⍥', '⍜', undefined],
	['KeyP', 'p', 'P', '◡', '◠', '⏨', '⌓'],
	['BracketLeft', '[', '{', '←', '⟨', '⦅', '⦃'],
	['BracketRight', ']', '}', '→', '⟩', '⦆', '⦄'],
	['KeyA', 'a', 'A', '⍺', '⍶', 'ɛ', undefined],
	['KeyS', 's', 'S', '⌈', '§', '↾', undefined],
	['KeyD', 'd', 'D', '⌊', '⸠', '⇂', undefined],
	['KeyF', 'f', 'F', '⍛', '∡', '∠', undefined],
	['KeyG', 'g', 'G', '∇', '⍢', '⫇', undefined],
	['KeyH', 'h', 'H', '∆', '⍙', '⊸', '⟜'],
	['KeyJ', 'j', 'J', '∘', '⍤', 'ᴊ', undefined],
	['KeyK', 'k', 'K', undefined, '⌸', undefined, undefined],
	['KeyL', 'l', 'L', '⎕', '⌷', undefined, undefined],
	['Semicolon', ';', ':', '⍎', '≡', '⍮', '⍠'],
	['Quote', '\'', '"', '⍕', '≢', '⍘', '⍞'],
	['Backslash', '\\', '|', '⊢', '⊣', '⊩', '⫣'],
	['KeyZ', 'z', 'Z', '⊂', '⊆', '⊏', 'ᑣ'],
	['KeyX', 'x', 'X', '⊃', '⊇', '⊐', 'ᑒ'],
	['KeyC', 'c', 'C', '∩', '⍝', '⟃', '⟄'],
	['KeyV', 'v', 'V', '∪', '⁖', '⫤', undefined],
	['KeyB', 'b', 'B', '⊥', '∵', '⇇', undefined],
	['KeyN', 'n', 'N', '⊤', '·', '↚', undefined],
	['KeyM', 'm', 'M', '«', '»', '↩', undefined],
	['Comma', ',', '<', '⍪', 'ᑈ', '⊲', undefined],
	['Period', '.', '>', '∙', 'ᐵ', '⊳', '■'],
	['Slash', '/', '?', '⌿', undefined, undefined, '⍰'],
	['Space', 'Space', 'Space', '`', '‿', undefined, undefined],
].map(([code, sym, symS, symP, symPS, symPP, symPPS]) => ({ code, sym, symS, symP, symPS, symPP, symPPS }));

const colors = {
	other: 'unset',
	syntax: 'unset',
	number: '#ea0027',
	string: '#0079d3',
	stringEscape: '#0266b3',
	arrayName: '#ff4500',
	primArray: '#cc3600',
	functionName: '#46d160',
	primFunction: '#349e48',
	adverbName: '#ff66ac',
	primAdverb: '#cc5289',
	conjunctionName: '#ffd635',
	primConjunction: '#ccac2b',
	comment: '#014980',
};

async function highlight(code: string, output: HTMLPreElement) {
	const pairs = zip(await tinyapl.splitString(code), await tinyapl.highlight(code));
	output.innerHTML = '';
	for (const [t, c] of pairs) {
		if (t === '\n') {
			output.appendChild(document.createElement('br'));
			// Appending a zero-width non-joiner forces the line break to be rendered
			// and the highlighted text to expand properly
			output.appendChild(document.createTextNode('\u200c'));
			continue;
		}
		const span = document.createElement('span');
		span.className = 'char ' + tinyapl.colorsInv[c];
		span.style.color = colors[tinyapl.colorsInv[c] as keyof typeof colors];
		span.innerText = t;
		output.appendChild(span);
	}
}

async function highlightInput() {
	highlight(input.value, highlighted);
	highlighted.scrollLeft = input.scrollLeft;
}

function insertText(str: string) {
	input.setRangeText(str, input.selectionStart ?? input.value.length - 1, input.selectionEnd ?? input.value.length - 1, "end");
	input.focus();
	highlightInput();
}

const io = new class IO {
	#input: ((i: string) => PromiseLike<void>)[] = [];
	#output: ((o: string) => PromiseLike<void>)[] = [];
	#error: ((e: string) => PromiseLike<void>)[] = [];
	rInput(l: (i: string) => PromiseLike<void>) { this.#input.push(l); }
	rOutput(l: (o: string) => PromiseLike<void>) { this.#output.push(l); }
	rError(l: (e: string) => PromiseLike<void>) { this.#error.push(l); }
	done() { this.#input = []; this.#output = []; this.#error = []; }
	async input() { const i = window.prompt('Input') ?? ''; for (const l of this.#input) await l(i); return i; }
	async output(what: string) { for (const l of this.#output) await l(what); }
	async error(what: string) { for (const l of this.#error) await l(what); }
};

for (const k of (['syntax', 'identifiers', 'arrays', 'functions', 'adverbs', 'conjunctions'] as const as readonly (keyof typeof tinyapl.glyphs)[])) {
	for (const i of tinyapl.glyphs[k]) {
		let v, p;
		if (v = keyboard.find(k => k.symP === i)) p = `${prefix.sym}${v.sym}`;
		else if (v = keyboard.find(k => k.symPS === i)) p = `${prefix.sym}${v.symS}`;
		else if (v = keyboard.find(k => k.symPP === i)) p = `${prefix.sym}${prefix.sym}${v.sym}`;
		else if (v = keyboard.find(k => k.symPPS === i)) p = `${prefix.sym}${prefix.sym}${v.symS}`;
		const b = document.createElement('button');
		b.textContent = i;
		if (p !== undefined) b.title = `Input: ${p}`;
		b.addEventListener('click', () => { insertText(i); });
		buttons.appendChild(b);
	}
	buttons.appendChild(document.createElement('br'));
}

const context = await tinyapl.newContext(io.input.bind(io), io.output.bind(io), io.error.bind(io), {
	Debug: {
		type: 'function',
		repr: '⎕Debug',
		monad: async (ea: tinyapl.ExtraArgs, y: tinyapl.Noun) => { console.log('monad call', y, 'extra args:', ea); return y; },
		dyad: async (ea: tinyapl.ExtraArgs, x: tinyapl.Noun, y: tinyapl.Noun) => { console.log('dyad call', x, y, 'extra args:', ea); return y; },
	},
	CreateImage: quads.qCreateImage,
	DisplayImage: quads.qDisplayImage,
	PlayAnimation: quads.qPlayAnimation,
	ScatterPlot: quads.qScatterPlot,
	PlayAudio: quads.qPlayAudio,
	Fetch: quads.qFetch,
	_Graph: quads.qGraph,
});

function el<E extends HTMLElement>(tag: string, cls: string, contents: string | Node | Node[]) {
	const el = document.createElement(tag) as E;
	el.className = cls;
	if (typeof contents === 'string') el.textContent = contents;
	else if (Array.isArray(contents)) for (const c of contents) el.appendChild(c);
	else el.appendChild(contents);
	return el;
}

const div = (cls: string, contents: string) => el<HTMLDivElement>('div', cls, contents);
const span = (cls: string, contents: string) => el<HTMLSpanElement>('span', cls, contents);

function clickableEl<E extends HTMLElement>(tag: string, cls: string, contents: string | Node | Node[], clickedContents: string) {
	const e = el<E>(tag, cls, contents);
	e.addEventListener('click', () => {
		if (input.value.trim() == '') {
			input.value = clickedContents;
			input.focus();
			highlightInput();
		}
	});
	return e;
}

const clickableDiv = (cls: string, contents: string | Node | Node[], clickedContents: string) => clickableEl<HTMLDivElement>('div', cls, contents, clickedContents);
const clickableSpan = (cls: string, contents: string | Node | Node[], clickedContents: string) => clickableEl<HTMLSpanElement>('span', cls, contents, clickedContents);

const selfClickableDiv = (cls: string, contents: string) => clickableEl<HTMLDivElement>('div', cls, contents, contents);
const selfClickableSpan = (cls: string, contents: string) => clickableEl<HTMLSpanElement>('span', cls, contents, contents);

const images: Record<number, HTMLCanvasElement> = {};

const ranCode: string[] = [];
let lastIndex = 0;

async function fancyShow(result: tinyapl.Value, depth: number = 0): Promise<Node> {
	const fsScalar = (v: tinyapl.ScalarValue) => {
		if (typeof v === 'object' && !Array.isArray(v) && v.type !== 'struct') return fancyShow(v, depth + 1);
		return fancyShow({ type: 'array', shape: [], contents: [v] }, depth + 1);
	};

	if (depth >= 5) return document.createTextNode(await tinyapl.show(result));

	if (result.type === 'array' && result.shape.length === 1 && result.contents.length !== 0 && !result.contents.every(x => typeof x === 'string')) {
		const table = document.createElement('table');
		table.className = 'vector';
		const tbody = document.createElement('tbody');
		table.appendChild(tbody);
		const tr = document.createElement('tr');
		tbody.appendChild(tr);
		for (let x = 0; x < result.shape[0]; x++) {
			const el = result.contents[x];
			const td = document.createElement('td');
			td.appendChild(await fsScalar(el));
			tr.appendChild(td);
		}
		return table;
	} else if (result.type === 'array' && result.shape.length === 2 && result.contents.length !== 0) {
		const table = document.createElement('table');
		table.className = 'matrix';
		const tbody = document.createElement('tbody');
		table.appendChild(tbody);
		const allNumbers = result.contents.every(x => Array.isArray(x));
		const allShown = await Promise.all(result.contents.map(e => tinyapl.show({ type: 'array', shape: [], contents: [e] })))
		const maxLength = Math.max(...allShown.map(x => x.length));
		for (let y = 0; y < result.shape[0]; y++) {
			const tr = document.createElement('tr');
			tbody.appendChild(tr);
			for (let x = 0; x < result.shape[1]; x++) {
				const idx = y * result.shape[1] + x;
				const td = document.createElement('td');
				if (allNumbers) {
					const el = allShown[idx];
					td.append(el.padStart(maxLength));
				} else {
					const el = result.contents[idx];
					td.appendChild(await fsScalar(el));
				}	
				tr.appendChild(td);
			}
		}
		return table;
	} else if (result.type === 'array' && result.shape.length === 3 && result.contents.length !== 0) {
		const [a, b, c] = result.shape;
		const table = document.createElement('table');
		table.className = 'cube';
		const tbody = document.createElement('tbody');
		table.appendChild(tbody);
		const allNumbers = result.contents.every(x => Array.isArray(x));
		const allShown = await Promise.all(result.contents.map(e => tinyapl.show({ type: 'array', shape: [], contents: [e] })))
		const maxLength = Math.max(...allShown.map(x => x.length));
		for (let y = 0; y < a * b; y++) {
			const tr = document.createElement('tr');
			tbody.appendChild(tr);
			for (let x = 0; x < c + a - 1; x++) {
				const c0 = Math.floor(y / b);
				const c1 = y % b;
				const c2 = x - c0;
				const td = document.createElement('td');
				if (0 <= c0 && c0 < a && 0 <= c1 && c1 < b && 0 <= c2 && c2 < c) {
					const idx = c0 * b * c + c1 * c + c2;
					if (allNumbers) {
						const el = allShown[idx];
						td.append(el.padStart(maxLength));
					} else {
						const el = result.contents[idx];
						td.appendChild(await fsScalar(el));
					}
				} else {
					td.classList.add('filler');
				}
				tr.appendChild(td);
			}
		}
		return table;
	} else if (result.type === 'array' && result.shape.length === 0 && typeof result.contents[0] === 'object' && !Array.isArray(result.contents[0]) && (result.contents[0] as tinyapl.ScalarValue & { type: string }).type === 'struct') {
		const struct = result.contents[0] as tinyapl.Struct;
		const details = document.createElement('details');
		details.open = depth === 0;
		const summary = document.createElement('summary');
		summary.textContent = 'struct';
		details.appendChild(summary);
		const table = document.createElement('table');
		const tbody = document.createElement('tbody');
		table.appendChild(tbody);
		for (const [k, v] of Object.entries(struct.entries)) {
			if (v[0] === 'private') continue;
			const tr = document.createElement('tr');
			const tdName = document.createElement('td');
			tdName.textContent = k;
			tr.appendChild(tdName);
			const tdArrow = document.createElement('td');
			tdArrow.textContent = await tinyapl.varArrow(v[0]);
			tr.appendChild(tdArrow);
			const tdValue = document.createElement('td');
			tdValue.appendChild(await fsScalar(v[1]));
			tr.appendChild(tdValue);
			tbody.appendChild(tr);
		}
		details.appendChild(table);
		return details;
	} else if (result.type === 'dictionary' && result.entries.length !== 0) {
		const table = document.createElement('table');
		table.className = 'dictionary';
		const tbody = document.createElement('tbody');
		table.appendChild(tbody);
		for (const [k, v] of result.entries) {
			const tr = document.createElement('tr');
			const tdName = document.createElement('td');
			tdName.appendChild(await fsScalar(k));
			tr.appendChild(tdName);
			const tdColon = document.createElement('td');
			tdColon.textContent = ':';
			tr.appendChild(tdColon);
			const tdValue = document.createElement('td');
			tdValue.appendChild(await fsScalar(v));
			tr.appendChild(tdValue);
			tbody.appendChild(tr);
		}
		return table;
	}
	return document.createTextNode(await tinyapl.show(result));
}

async function runCode(code: string) {
	let d: HTMLDivElement;

	const endDiv = () => {
		if (d.textContent!.trim() === '') try { output.removeChild(d); } catch (_) { };
		if (d.textContent!.at(-1) === '\n') d.textContent = d.textContent!.slice(0, -1);
	};

	const newDiv = () => {
		d = div('quad', '');
		output.appendChild(d);
	};

	const createImage = (id: number | undefined, width: number, height: number) => {
		endDiv();
		const canvas = document.createElement('canvas');
		canvas.className = 'image';
		canvas.width = width;
		canvas.height = height;
		if (id !== undefined) {
			canvas.dataset.tinyaplId = id.toString();
			images[id] = canvas;
		}
		output.appendChild(canvas);
		newDiv();
		return canvas;
	};

	ranCode.push(code);
	lastIndex = ranCode.length;

	button.disabled = true;
	const in1 = div('executed', '');
	output.appendChild(in1);
	const pad = span('pad', '');
	const loader = div('loader', '');
	pad.appendChild(loader);
	in1.appendChild(pad);
	in1.appendChild(selfClickableSpan('code', code));
	newDiv();
	io.rInput(async what => { d.innerText += what + '\n'; });
	io.rOutput(async what => { d.innerText += what; });
	io.rError(async what => { d.innerText += what; });
	quads.rCreateImage(async (id, width, height) => { createImage(id, width, height); });
	quads.rDisplayImage(async (id, data) => {
		let canvas;
		if (id !== undefined)
			canvas = images[id];
		else
			canvas = createImage(id, data.width, data.height);
		const ctx = canvas.getContext('2d')!;
		ctx.clearRect(0, 0, canvas.width, canvas.height);
		ctx.putImageData(data, 0, 0);
	});
	quads.rPlayAnimation(async (delay, data) => {
		endDiv();
		const gif = await encodeGIF({ width: data[0].width, height: data[0].height, frames: await Promise.all(data.map(async d => ({ data: await createImageBitmap(d), delay: delay * 1000 }))) });
		const img = document.createElement('img');
		img.className = 'image';
		img.src = URL.createObjectURL(new Blob([gif], { type: 'image/gif' }));
		output.appendChild(img);
		newDiv();
	});
	quads.rScatterPlot(async (xs, ys, mode) => {
		endDiv();
		const traces = xs.map((x, i) => ({ x, y: ys[i], mode: mode as _Plotly.ScatterData['mode'], type: 'scatter', line: { shape: 'spline' } } satisfies Partial<_Plotly.ScatterData> as Partial<_Plotly.ScatterData>));
		const d = div('plot', '');
		output.appendChild(d);
		Plotly.newPlot(d, traces, { font: { family: 'var(--font-mono)' }, showlegend: false }, { responsive: true });
		newDiv();
	});
	quads.rGraph(async (data, labels) => {
		endDiv();
		const traces = zip((console.log(data), data), (console.log(labels), labels)).map(([ps, l]) => (console.log(ps), { x: ps.map(x => x[0]), y: ps.map(x => x[1]), mode: 'lines', line: { shape: 'spline' }, name: l }));
		console.log(traces);
		const d = div('plot', '');
		output.appendChild(d);
		Plotly.newPlot(d, traces, { font: { family: 'var(--font-mono)' }, showlegend: true }, { responsive: true });
		newDiv();
	});
	quads.rPlayAudio(async buf => {
		endDiv();
		const audio = document.createElement('audio');
		audio.controls = true;
		audio.autoplay = true;
		const blob = new Blob([buf], { type: 'audio/wav' });
		const url = URL.createObjectURL(blob);
		audio.src = url;
		output.appendChild(audio);
		newDiv();
	});
	const result = await tinyapl.runCode(context, code);
	io.done();
	quads.dCreateImage();
	quads.dDisplayImage();
	quads.dPlayAudio();
	quads.dPlayAnimation();
	quads.dScatterPlot();
	quads.dGraph();
	endDiv();
	if ('code' in result) output.appendChild(div('error', await tinyapl.show(result)));
	else if (fancyarrays.checked) output.appendChild(clickableDiv('result', await fancyShow(result), await tinyapl.repr(result)));
	else output.appendChild(clickableDiv('result', await tinyapl.show(result), await tinyapl.repr(result)));
	loader.remove();
	button.disabled = false;
}

async function run() {
	const v = input.value;
	input.value = '';
	highlightInput();
	await runCode(v);
}

let keyboardState = 0;

function loadLast() {
	if (lastIndex >= ranCode.length) lastIndex = ranCode.length;
	if (lastIndex < 0) lastIndex = 0;
	if (lastIndex === ranCode.length) input.value = '';
	else input.value = ranCode[lastIndex];
	highlightInput();
}

button.addEventListener('click', () => run());
input.addEventListener('keydown', evt => {
	if (keyboardState < 2 && evt.code === prefix.code && !evt.shiftKey) {
		keyboardState++
		evt.preventDefault();
	} else if (keyboardState !== 0 && !evt.altKey && !evt.ctrlKey && !evt.metaKey) {
		const v = keyboard.find(k => k.code == evt.code);
		if (v) {
			const t = keyboardState === 2 ? (evt.shiftKey ? v.symPPS : v.symPP) : (evt.shiftKey ? v.symPS : v.symP);
			insertText(t ?? evt.key);
			keyboardState = 0;
			evt.preventDefault();
		}
	} else if (evt.key === 'Enter' && !evt.shiftKey) {
		evt.preventDefault();
		if (!button.disabled) return run();
	} else if (evt.key === 'ArrowUp' && evt.shiftKey && !evt.altKey && !evt.ctrlKey && !evt.metaKey) {
		lastIndex--;
		loadLast();
	} else if (evt.key === 'ArrowDown' && evt.shiftKey && !evt.altKey && !evt.ctrlKey && !evt.metaKey) {
		lastIndex++;
		loadLast();
	}
});
input.addEventListener('input', () => highlightInput());
input.addEventListener('scroll', () => highlightInput());

infobutton.addEventListener('click', () => {
	info.classList.toggle('shown');
});

document.querySelector('#prefixkey')!.textContent = prefix.sym;

for (const k of document.querySelectorAll<HTMLPreElement>('.hl')) highlight(k.textContent ?? '', k);

document.querySelector('#loading')!.remove();
button.disabled = false;
infobutton.disabled = false;
highlightInput(); // in case the input is pre-filled

const search = new URLSearchParams(window.location.search);
for (const line of search.getAll('run')) await runCode(decodeURIComponent(line));
