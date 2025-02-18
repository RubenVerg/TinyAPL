import { Glyph, Info, Primitive, Quad, Pages } from './types.d.ts';
import { readFrontmatter, renderMarkdown } from './markdown.ts';
import { recordGetter } from './utils.ts';
import { h, Fragment, JSXNode } from './deps/x/htm.ts';

const force = async (el: JSX.Element) => {
	if (el.children) {
		for (let idx = 0; idx < el.children.length; idx++) {
			const c = el.children[idx];
			if (c instanceof JSXNode) {
				if (c.tag instanceof Function)
					el.children[idx] = await c.tag({ ...c.props, ...(c.children && { children: c.children }) });
				await force(el.children[idx] as typeof c);
			}
		}
	};
	return el;
};

function readGlyph(path: string, src: string): Glyph {
	const frontmatter = recordGetter(readFrontmatter(src));
	const body = renderMarkdown(src);
	const name = frontmatter.get<string>('name') ?? (console.warn(`No name for ${path}`), path);
	const glyph = frontmatter.get<string>('glyph') ?? (console.warn(`No glyph for ${name}`), '');
	const primitives = frontmatter.get<string[]>('primitives') ?? [];
	return {
		glyph,
		name,
		primitives,
		body,
	};
}

function readInfo(path: string, src: string): Info {
	const frontmatter = recordGetter(readFrontmatter(src));
	const body = renderMarkdown(src);
	const name = frontmatter.get<string>('name') ?? (console.warn(`No name for ${path}`), path);
	return {
		name,
		planned: frontmatter.get('planned') ?? false,
		body,
	};
}

function readPrimitive(path: string, src: string): Primitive {
	const frontmatter = recordGetter(readFrontmatter(src));
	const body = renderMarkdown(src);
	const name = frontmatter.get<string>('name') ?? (console.warn(`No name for ${path}`), path);
	const glyph = frontmatter.get<string>('glyph') ?? (console.warn(`No glyph for ${name}`), '');
	const pattern = frontmatter.get<string>('pattern') ?? (console.warn(`No pattern for ${name}`), '');
	return {
		glyph,
		pattern,
		name,
		planned: frontmatter.get('planned') ?? false,
		deprecated: frontmatter.get('deprecated') ?? false,
		body,
	};
}

function readQuad(path: string, src: string): Quad {
	const frontmatter = recordGetter(readFrontmatter(src));
	const body = renderMarkdown(src);
	const name = frontmatter.get<string>('name') ?? (console.warn(`No name for ${path}`), path);
	const glyph = frontmatter.get<string>('glyph') ?? (console.warn(`No glyph for ${name}`), '');
	const pattern = frontmatter.get<string>('pattern') ?? (console.warn(`No pattern for ${name}`), '');
	const category = frontmatter.get<string>('category') ?? (console.warn(`No category for ${name}`), 'Core');
	return {
		glyph,
		pattern,
		name,
		category,
		planned: frontmatter.get('planned') ?? false,
		deprecated: frontmatter.get('deprecated') ?? false,
		body,
	};
}

let pages: Pages = { index: h(Fragment, {}), glyphs: {}, info: {}, primitives: {}, quads: {} };

export async function loadPages(): Promise<void> {
	pages.index = renderMarkdown(await Deno.readTextFile('pages/index.mdx'));

	for await (const file of Deno.readDir('pages/glyphs')) {
		if (file.isFile && file.name.endsWith('.mdx')) {
			const path = file.name.slice(0, -4);
			pages.glyphs[path] = readGlyph(path, await Deno.readTextFile(`pages/glyphs/${file.name}`));
		}
	}

	for await (const file of Deno.readDir('pages/info')) {
		if (file.isFile && file.name.endsWith('.mdx')) {
			const path = file.name.slice(0, -4);
			pages.info[path] = readInfo(path, await Deno.readTextFile(`pages/info/${file.name}`));
		}
	}

	for await (const file of Deno.readDir('pages/primitives')) {
		if (file.isFile && file.name.endsWith('.mdx')) {
			const path = file.name.slice(0, -4);
			pages.primitives[path] = readPrimitive(path, await Deno.readTextFile(`pages/primitives/${file.name}`));
		}
	}

	for await (const file of Deno.readDir('pages/quads')) {
		if (file.isFile && file.name.endsWith('.mdx')) {
			const path = file.name.slice(0, -4);
			pages.quads[path] = readQuad(path, await Deno.readTextFile(`pages/quads/${file.name}`))
		}
	}
}

export async function forcePages(): Promise<void> {
	pages.index = await force(pages.index);
	pages.info = Object.fromEntries(await Promise.all(Object.entries(pages.info).map(async ([k, v]) => [k, { ...v, body: await force(v.body) }])));
	pages.primitives = Object.fromEntries(await Promise.all(Object.entries(pages.primitives).map(async ([k, v]) => [k, { ...v, body: await force(v.body) }])));
	pages.quads = Object.fromEntries(await Promise.all(Object.entries(pages.quads).map(async ([k, v]) => [k, { ...v, body: await force(v.body) }])));
}

export function validatePages(): void {
	for (const { glyph, primitives } of Object.values(pages.glyphs))
		for (const p of primitives)
			if (!(p in pages.primitives)) throw new Error(`Primitive ${p} not found for glyph ${glyph}`);
	const allGlyphs = [...Object.values(pages.glyphs).map(g => g.glyph).join('')];
	const allGlyphPrimitives = Object.values(pages.glyphs).flatMap(p => p.primitives);
	for (const [p, { glyph, name }] of Object.entries(pages.primitives)) {
		for (const g of [...glyph]) {
			if (!allGlyphs.includes(g)) throw new Error(`Primitive ${name} has glyph ${g} which is not in the glyphs`);
			if (!Object.values(pages.glyphs).find(_ => _.glyph === g)!.primitives.includes(p)) throw new Error(`Primitive ${name} doesn't appear in the glyph page for ${g}`);
		}
		if (!allGlyphPrimitives.includes(p)) throw new Error(`Primitive ${name} is not in the glyphs`);
	}
}

try {
	const p = JSON.parse(await Deno.readTextFile('pages.json'), (k, v) => {
		if (k !== 'body') return v;
		return h('div', { dangerouslySetInnerHTML: { __html: v } });
	});
	pages = { ...p, index: p.index.body };
} catch {
	await loadPages();
}

export default pages;