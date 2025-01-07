/** @jsx h */
/** @jsxFrag Fragment */

import { Pages } from '../types.d.ts';

import { Fragment, h } from '../deps/x/htm.ts';

export interface GlyphsProps {
	pages: Pages;
}

function Primitives({ pages }: GlyphsProps) {
	return <>
		<h1>Glyphs</h1>
		
		<ul>
			{[...Object.entries(pages.glyphs)].sort(([_ak, av], [_bk, bv]) => av.name.localeCompare(bv.name)).map(([id, glyph]) => <li><a href={`/docs/glyph/${id}`}><code>{glyph.glyph}</code> {glyph.name}</a></li>)}
		</ul>
	</>;
}

export default Primitives;