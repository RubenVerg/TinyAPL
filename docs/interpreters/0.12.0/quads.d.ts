import * as tinyapl from './tinyapl.js';
export declare const rCreateImage: (l: (args_0: number, args_1: number, args_2: number) => Promise<void>) => void, dCreateImage: () => void, qCreateImage: tinyapl.Fun;
export declare const rDisplayImage: (l: (args_0: number | undefined, args_1: ImageData) => Promise<void>) => void, dDisplayImage: () => void, qDisplayImage: tinyapl.Fun;
export declare const rPlayAnimation: (l: (args_0: number, args_1: ImageData[]) => Promise<void>) => void, dPlayAnimation: () => void, qPlayAnimation: tinyapl.Fun;
export declare const rScatterPlot: (l: (args_0: number[][], args_1: number[][], args_2: string) => Promise<void>) => void, dScatterPlot: () => void, qScatterPlot: tinyapl.Fun;
export declare const rGraph: (l: (args_0: [number, number][][], args_1: string[]) => Promise<void>) => void, dGraph: () => void, qGraph: tinyapl.Adv;
export declare const rPlayAudio: (l: (args_0: ArrayBuffer) => Promise<void>) => void, dPlayAudio: () => void, qPlayAudio: tinyapl.Fun;
export declare const qFetch: tinyapl.Fun;
