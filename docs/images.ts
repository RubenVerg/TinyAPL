export function imageName(pattern: string) {
	return `./assets/gen_img_${[...new TextEncoder().encode(pattern)].map(n => n.toString(16).padStart(2, '0')).join('.')}.png`;
}

export async function fullImageForPattern(pattern: string) {
	return await Deno.readFile(imageName(pattern));
}
