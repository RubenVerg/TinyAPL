/** @jsx h */
/** @jsxFrag Fragment */

import { DeprecatedAlert, DeprecatedBadge, PlannedAlert, PlannedBadge } from './Banners.tsx';
import { Primitive, type Pages } from '../types.d.ts';

import { Fragment, h } from '../deps/x/htm.ts';

export interface PrimitivePageProps {
	primitive: Primitive;
	pages: Pages;
}

function PrimitivePage({ primitive, pages }: PrimitivePageProps) {
	const id = Object.entries(pages.primitives).find(([_k, v]) => v.name == primitive.name)![0];
	const glyphs = [...primitive.glyph].map(gl => Object.entries(pages.glyphs).find((([_k, v]) => v.glyph === gl))![0])
	const otherPrimitives = Object.values(pages.glyphs)
		.filter(gl => primitive.glyph.includes(gl.glyph))
		.flatMap(gl => gl.primitives.filter(p => p !== id))
		.map(p => [p, pages.primitives[p]!.name, pages.primitives[p]!.pattern]);
	return <>
		<h1>{primitive.planned && <PlannedBadge />}{primitive.deprecated && <DeprecatedBadge />}<code>{[...primitive.glyph].map((char, idx) => <a class='link-underline link-underline-opacity-0' href={`../glyph/${glyphs[idx]}`}>{char}</a>)}</code> {primitive.name} {otherPrimitives.length ? <div class='dropdown d-inline-block'>
			<a class='btn btn-small dropdown-toggle' href='#' role='button' data-bs-toggle='dropdown' aria-expanded='false'>Also on this glyph</a>

			<ul class='dropdown-menu'>
				{otherPrimitives.map(([key, name, pattern]) => <li><a class='dropdown-item' href={`./${key}`}><code>{pattern}</code>{' '}&mdash; {name}</a></li>)}
			</ul>
		</div> : ''} <code class='float-end'>{primitive.pattern}</code></h1>

		{primitive.planned && <PlannedAlert />}

		{primitive.deprecated && <DeprecatedAlert />}

		{primitive.body}
	</>;
}

export default PrimitivePage;