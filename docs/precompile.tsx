import pages, { forcePages, loadPages, validatePages } from './pages.ts';
import interpreters, { loadInterpreters } from './interpreters.ts';
import { imageName, fullImageForPattern } from './generate_images.ts';
import { exists } from './deps/std/fs.ts';

await loadPages();
validatePages();
await forcePages();
await loadInterpreters();

for (const { pattern } of Object.values(pages.primitives)) {
	const name = imageName(pattern);
	if (await exists(name)) await Deno.remove(name);
	await Deno.writeFile(name, await fullImageForPattern(pattern));
}

for (const { pattern } of Object.values(pages.quads)) {
	const name = imageName(pattern);
	if (await exists(name)) await Deno.remove(name);
	await Deno.writeFile(name, await fullImageForPattern(pattern));
}

await Deno.writeTextFile('pages.json', JSON.stringify({
	index: { body: pages.index.toString() },
	...Object.fromEntries(Object.entries(pages).filter(([k]) => k !== 'index').map(([k, v]: [string, Record<string, { body: JSX.Element }>]) => { return [ k, Object.fromEntries(Object.entries(v).map(([k1, v1]) => [ k1, { ...v1, body: v1.body.toString() } ])) ]; })),
}));
await Deno.writeTextFile('interpreters.json', JSON.stringify(interpreters));