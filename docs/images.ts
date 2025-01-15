import * as tinyapl from './interpreters/latest/tinyapl.js';

import { createCanvas, Image, Fonts } from './deps/jsr/gfx/canvas.ts';
import { exists } from './deps/std/fs.ts'

Fonts.register(await Deno.readFile('./assets/TinyAPL386.ttf'), 'TinyAPL386');

function zip<A, B>(as: A[], bs: B[]): [A, B][] {
	return [...as, ...bs].slice(0, Math.min(as.length, bs.length)).map((_, idx) => [as[idx], bs[idx]]);
}

const colors = {
	other: '#000000',
	syntax: '#000000',
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

async function imageForPattern(pattern: string) {
	const padding = 25;

	pattern = pattern.replace(/^r←/u, '');

	const cs = zip(await tinyapl.splitString(pattern), await tinyapl.highlight(pattern).then(xs => xs.map(x => colors[tinyapl.colorsInv[x] as keyof typeof colors])));

	const canvas = createCanvas(100, 100);
	const context = canvas.getContext('2d');
	context.fillStyle = '#ffffff';
	context.fillRect(0, 0, canvas.width, canvas.height);
	
	context.font = `200px TinyAPL386`;
	const totalSize = context.measureText(pattern);
	canvas.resize(Math.ceil(totalSize.width + padding * 2), Math.ceil(totalSize.fontBoundingBoxAscent + totalSize.fontBoundingBoxDescent + padding * 2));

	context.font = `200px TinyAPL386`;
	let x = padding;
	for (const [char, col] of cs) {
		context.fillStyle = col;
		context.fillText(char, x, padding + totalSize.fontBoundingBoxAscent);
		x += context.measureText(char).width;
	}

	return canvas.encode('png');
}

const banner = await Image.load('./assets/banner.png');

export function imageName(pattern: string) {
	return `./assets/gen_img_${[...new TextEncoder().encode(pattern)].map(n => n.toString(16).padStart(2, '0')).join('.')}.png`;
}

export async function fullImageForPattern(pattern: string) {
	if (await exists(imageName(pattern))) return await Deno.readFile(imageName(pattern));

	const width = banner.width, height = 500;
	const bottomHeight = banner.height, topHeight = height - bottomHeight;

	const canvas = createCanvas(width, height);
	const context = canvas.getContext('2d');
	context.fillStyle = '#ffffff';
	context.fillRect(0, 0, width, height);

	context.drawImage(banner, 0, topHeight);

	const patternImage = new Image(await imageForPattern(pattern));
	const scaleFactor = Math.min(width / patternImage.width, topHeight / patternImage.height);
	context.drawImage(
		patternImage, 
		Math.round((width - scaleFactor * patternImage.width) / 2),
		Math.round((topHeight - scaleFactor * patternImage.height) / 2),
		Math.round(scaleFactor * patternImage.width),
		Math.round(scaleFactor * patternImage.height),
	);

	return canvas.encode('png');
}
