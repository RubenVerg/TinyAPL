/** @jsx h */

import { h } from '../deps/x/htm.ts';

import * as tinyapl from '../interpreters/latest/tinyapl.js';

const colors = {
	other: 'text-body-secondary',
	syntax: 'text-body-emphasis',
	number: 'text-danger-emphasis',
	string: 'text-primary',
	stringEscaoe: 'text-primary-emphasis',
	arrayName: 'text-danger',
	primArray: 'text-danger',
	functionName: 'text-success',
	primFunction: 'text-success',
	adverbName: 'text-info',
	primAdverb: 'text-info',
	conjunctionName: 'text-warning',
	primConjunction: 'text-warning',
	comment: 'text-secondary',
}

function zip<A, B>(as: A[], bs: B[]): [A, B][] {
	return [...as, ...bs].slice(0, Math.min(as.length, bs.length)).map((_, idx) => [as[idx], bs[idx]]);
}

export interface HighlightCodeProps {
	code: string;
}

async function HighlightCode({ code: c }: HighlightCodeProps) {
	const code = <code></code>;
	const chars = await tinyapl.splitString(c);
	const hl = await tinyapl.highlight(c);
	for (const [t, c] of zip(chars, hl)) code.children.push(
		<span class={`hl-char hl-char-${tinyapl.colorsInv[c]} ${colors[tinyapl.colorsInv[c] as keyof typeof colors]}`}>{t}</span>
	);
	return code;
}

export default HighlightCode;
