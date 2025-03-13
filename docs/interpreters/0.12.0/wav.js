export const encode = (sampleRate, channelData) => {
    // adapted from https://www.npmjs.com/package/wav-encoder by @mohayonao
    const channelLength = channelData[0].length;
    const channels = channelData.length;
    const bitDepth = 16;
    const bytes = bitDepth >> 3;
    const length = channelLength * channels * bytes;
    const view = new DataView(new Uint8Array(44 + length).buffer);
    let pos = 0;
    const string = (s) => { for (const ch of s)
        view.setUint8(pos++, ch.charCodeAt(0)); };
    const uint16 = (value) => { view.setUint16(pos, value, true); pos += 2; };
    const uint32 = (value) => { view.setUint32(pos, value, true); pos += 4; };
    const pcm16 = (value) => {
        value = Math.max(-1, Math.min(value, 1));
        value = value < 0 ? value * 32768 : value * 32767;
        value = Math.round(value) | 0;
        view.setInt16(pos, value, true);
        pos += 2;
    };
    string('RIFF');
    uint32(view.buffer.byteLength - 8);
    string('WAVE');
    string('fmt ');
    uint32(16);
    uint16(1);
    uint16(channels);
    uint32(sampleRate);
    uint32(sampleRate * channels * bytes);
    uint16(channels * bytes);
    uint16(bitDepth);
    string('data');
    uint32(length);
    for (let i = 0; i < channelLength; i++)
        for (let ch = 0; ch < channels; ch++)
            pcm16(channelData[ch][i]);
    return view.buffer;
};
