/**
 * Data.Binary serialization library implemented in typescript
 */
import { absurd } from './lib';

type HaskellPointer = number;

export class DecoderBase<A> {
  // @ts-ignore
  readonly _A: A;

  encode(value: A): Uint8Array {
    const decoder = this as Decoder<A>;
    const size = computeSize(decoder, value);
    const u8array = new Uint8Array(size);
    runEncoder(decoder, u8array, 0, value);
    return u8array;
  }

  decode(mem: Uint8Array): A {
    const decoder = this as Decoder<A>;
    const [resultVal, _ptr] = runDecoder(decoder, mem, 0);
    return resultVal;
  }
}

export type Decoder<A> =
  | Int8Decoder<A>
  | Int16Decoder<A>
  | Int32Decoder<A>
  | Int64Decoder<A>
  | Word8Decoder<A>
  | Word16Decoder<A>
  | Word32Decoder<A>
  | Word64Decoder<A>
  | Float32Decoder<A>
  | Float64Decoder<A>
  | Uint8ArrayDecoder<A>
  | StringDecoder<A>
  | ArrayDecoder<A>
  | RecordDecoder<A>
  | OneOfDecoder<A>
  | RecursiveDecoder<A>
  | TupleDecoder<A>
;

export class Int8Decoder<A> extends DecoderBase<A> {}
export class Int16Decoder<A> extends DecoderBase<A> {}
export class Int32Decoder<A> extends DecoderBase<A> {}
export class Int64Decoder<A> extends DecoderBase<A> {}
export class Word8Decoder<A> extends DecoderBase<A> {}
export class Word16Decoder<A> extends DecoderBase<A> {}
export class Word32Decoder<A> extends DecoderBase<A> {}
export class Word64Decoder<A> extends DecoderBase<A> {}
export class Float32Decoder<A> extends DecoderBase<A> {}
export class Float64Decoder<A> extends DecoderBase<A> {}
export class Uint8ArrayDecoder<A> extends DecoderBase<A> {}
export class StringDecoder<A> extends DecoderBase<A> {}
export class ArrayDecoder<A> extends DecoderBase<A> {
  constructor(
    readonly _element: Decoder<any>,
  ) { super(); }
}
export class RecordDecoder<A> extends DecoderBase<A> {
  constructor(
    readonly _description: Record<string, Decoder<any>>,
  ) { super(); }
}
export class OneOfDecoder<A> extends DecoderBase<A> {
  constructor(
    readonly _alternatives: Record<number, Decoder<any>>,
  ) { super(); }
}
export class RecursiveDecoder<A> extends DecoderBase<A> {
  constructor(
    public _self: Decoder<any>,
  ) { super(); }
}
export class TupleDecoder<A> extends DecoderBase<A> {
  constructor(
    readonly _tuple: Decoder<any>[],
  ) { super(); }
}
export function computeSize<A>(
  decoder: Decoder<A>,
  value: A
): number {
  if (decoder instanceof Int8Decoder) {
    return 1;
  }
  if (decoder instanceof Int16Decoder) {
    return 2;
  }
  if (decoder instanceof Int32Decoder) {
    return 4;
  }
  if (decoder instanceof Int64Decoder) {
    return 8;
  }
  if (decoder instanceof Word8Decoder) {
    return 1;
  }
  if (decoder instanceof Word16Decoder) {
    return 2;
  }
  if (decoder instanceof Word32Decoder) {
    return 4;
  }
  if (decoder instanceof Word64Decoder) {
    return 8;
  }
  if (decoder instanceof Float32Decoder) {
    return 4;
  }
  if (decoder instanceof Float64Decoder) {
    return 8;
  }
  if (decoder instanceof StringDecoder) {
    const str = value as any as string;
    const lengthSize = 8; // How many bytes to encode array length
    const u8array = new TextEncoder().encode(str);
    return lengthSize + u8array.length;
  }
  if (decoder instanceof Uint8ArrayDecoder) {
    const u8array = value as any as Uint8Array;
    const lengthSize = 8; // How many bytes to encode array length
    return lengthSize + u8array.length;
  }
  if (decoder instanceof ArrayDecoder) {
    const array = value as any as any[];
    const lengthSize = 8; // How many bytes to encode array length
    return array.reduce((acc, v) => acc + computeSize(decoder._element, v), lengthSize);
  }
  if (decoder instanceof RecordDecoder) {
    const obj = value as any as Record<any,any>;
    return Object.keys(decoder._description).reduce(((acc, k) =>
      acc + computeSize(decoder._description[k], obj[k])
      ), 0
    );
  }
  if (decoder instanceof OneOfDecoder) {
    const obj = value as any as { tag: number };
    const dscrSize = discriminatorSize(Object.keys(decoder._alternatives).length);
    return dscrSize + computeSize(decoder._alternatives[obj.tag], obj);
  }
  if (decoder instanceof RecursiveDecoder) {
    return computeSize(decoder._self, value);
  }
  if (decoder instanceof TupleDecoder) {
    const tupleVal = value as any as any[];
    return decoder._tuple.reduce((acc, v, i) => acc + computeSize(v, tupleVal[i]), 0);
  }
  return absurd(decoder);
}

export function runDecoder<A>(
  decoder: Decoder<A>,
  mem: Uint8Array,
  ptr: HaskellPointer,
): [A, HaskellPointer] {
  const view = new DataView(mem.buffer);
  if (decoder instanceof Int8Decoder) {
    const value = view.getInt8(ptr) as any;
    return [value, ptr + 1];
  }
  if (decoder instanceof Int16Decoder) {
    const value = view.getInt16(ptr, true) as any;
    return [value, ptr + 2];
  }
  if (decoder instanceof Int32Decoder) {
    const value = view.getInt32(ptr, true) as any;
    return [value as any as A, ptr + 4];
  }
  if (decoder instanceof Int64Decoder) {
    const value = view.getBigInt64(ptr, true) as any;
    return [value, ptr + 8];
  }
  if (decoder instanceof Word8Decoder) {
    const result = mem[ptr] as any as A;
    return [result, ptr + 1];
  }
  if (decoder instanceof Word16Decoder) {
    const value = view.getUint8(ptr) as any;
    return [value, ptr + 1];
  }
  if (decoder instanceof Word32Decoder) {
    const value = view.getUint32(ptr, true) as any;
    return [value as any as A, ptr + 4];
  }
  if (decoder instanceof Word64Decoder) {
    const value = view.getBigUint64(ptr, true) as any;
    return [value, ptr + 8];
  }
  if (decoder instanceof Float32Decoder) {
    const value = view.getFloat32(ptr, true) as any;
    return [value, ptr + 4];
  }
  if (decoder instanceof Float64Decoder) {
    const value = view.getFloat64(ptr, true) as any;
    return [value, ptr + 8];
  }
  if (decoder instanceof StringDecoder) {
    const len = mem[ptr + 7] +
      (mem[ptr + 6] << 8) +
      (mem[ptr + 5] << 16) +
      (mem[ptr + 4] << 24) +
      (mem[ptr + 3] << 32) +
      (mem[ptr + 2] << 40) +
      (mem[ptr + 1] << 48) +
      (mem[ptr] << 56);
    const strView = mem.subarray(ptr + 8, ptr + 8 + len);
    const resultStr = new TextDecoder("utf8").decode(strView) as any as A;
    return [resultStr, ptr + 8 + len];
  }
  if (decoder instanceof Uint8ArrayDecoder) {
    const len = mem[ptr + 7] +
      (mem[ptr + 6] << 8) +
      (mem[ptr + 5] << 16) +
      (mem[ptr + 4] << 24) +
      (mem[ptr + 3] << 32) +
      (mem[ptr + 2] << 40) +
      (mem[ptr + 1] << 48) +
      (mem[ptr] << 56);
    const resultU8Arr = mem.subarray(ptr + 8, ptr + 8 + len) as any as A;
    return [resultU8Arr, ptr + 8 + len];
  }
  if (decoder instanceof ArrayDecoder) {
    const len = mem[ptr + 7] +
      (mem[ptr + 6] << 8) +
      (mem[ptr + 5] << 16) +
      (mem[ptr + 4] << 24) +
      (mem[ptr + 3] << 32) +
      (mem[ptr + 2] << 40) +
      (mem[ptr + 1] << 48) +
      (mem[ptr] << 56);
    const resultArr = [];
    let jx = ptr + 8;
    for (let i = 0; i < len; i++) {
      const [val, newIx] = runDecoder(decoder._element, mem, jx);
      resultArr.push(val);
      jx = newIx;
    }
    return [resultArr as any as A, jx];
  }
  if (decoder instanceof RecordDecoder) {
    let jx = ptr;
    const resultRec = Object.fromEntries(
      Object.entries(decoder._description).map(([k, dec]) => {
        const [val, newIx] = runDecoder(dec, mem, jx);
        jx = newIx;
        return [k, val];
      })
    );
    return [resultRec as any as A, jx];
  }
  if (decoder instanceof OneOfDecoder) {
    const dscrSize = discriminatorSize(Object.keys(decoder._alternatives).length);
    const [tag, ix1] = readDiscriminator(dscrSize, mem, ptr);
    const [oneValue, ix2] = runDecoder(decoder._alternatives[tag], mem, ix1);
    oneValue['tag'] = tag;
    return [oneValue, ix2];
  }
  if (decoder instanceof RecursiveDecoder) {
    return runDecoder(decoder._self, mem, ptr);
  }
  if (decoder instanceof TupleDecoder) {
    let jx = ptr;
    const resultTup = decoder._tuple.map(dec => {
      const [val, newIx] = runDecoder(dec, mem, jx);
      jx = newIx;
      return val;
    });
    return [resultTup as any as A, jx];
  }
  return absurd(decoder);
}

export function runEncoder<A>(
  decoder: Decoder<A>,
  mem: Uint8Array,
  ptr: HaskellPointer,
  value: A,
): HaskellPointer {
  const view = new DataView(mem.buffer);
  if (decoder instanceof Int8Decoder) {
    view.setInt8(ptr, value as number);
    return ptr + 1;
  }
  if (decoder instanceof Int16Decoder) {
    view.setInt16(ptr, value as number, true);
    return ptr + 2;
  }
  if (decoder instanceof Int32Decoder) {
    view.setInt32(ptr, value as number, true);
    return ptr + 4;
  }
  if (decoder instanceof Int64Decoder) {
    view.setBigInt64(ptr, value as bigint, true);
    return ptr + 8;
  }
  if (decoder instanceof Word8Decoder) {
    view.setUint8(ptr, value as number);
    return ptr + 1;
  }
  if (decoder instanceof Word16Decoder) {
    view.setUint16(ptr, value as number, true);
    return ptr + 2;
  }
  if (decoder instanceof Word32Decoder) {
    view.setUint32(ptr, value as number, true);
    return ptr + 4;
  }
  if (decoder instanceof Word64Decoder) {
    view.setBigUint64(ptr, value as bigint, true);
    return ptr + 8;
  }
  if (decoder instanceof Float32Decoder) {
    view.setFloat32(ptr, value as number, true);
    return ptr + 4;
  }
  if (decoder instanceof Float64Decoder) {
    view.setFloat64(ptr, value as number, true);
    return ptr + 8;
  }
  if (decoder instanceof StringDecoder) {
    const str = value as any as string;
    const encoder = new TextEncoder();
    const strView = encoder.encode(str);
    const len = strView.length;
    mem[ptr + 7] = len & 0xFF;
    mem[ptr + 6] = (len >> 8) & 0xFF;
    mem[ptr + 5] = (len >> 16) & 0xFF;
    mem[ptr + 4] = (len >> 24) & 0xFF;
    // mem[ptr + 3] = (len >> 32) & 0xFF;
    // mem[ptr + 2] = (len >> 40) & 0xFF;
    // mem[ptr + 1] = (len >> 48) & 0xFF;
    // mem[ptr] = (len >> 56) & 0xFF;
    mem.set(strView, ptr + 8);
    return ptr + 8 + len;
  }
  if (decoder instanceof Uint8ArrayDecoder) {
    const u8Array = value as any as Uint8Array;
    const len = u8Array.length;
    mem[ptr + 7] = len & 0xFF;
    mem[ptr + 6] = (len >> 8) & 0xFF;
    mem[ptr + 5] = (len >> 16) & 0xFF;
    mem[ptr + 4] = (len >> 24) & 0xFF;
    // mem[ptr + 3] = (len >> 32) & 0xFF;
    // mem[ptr + 2] = (len >> 40) & 0xFF;
    // mem[ptr + 1] = (len >> 48) & 0xFF;
    // mem[ptr] = (len >> 56) & 0xFF;
    mem.set(u8Array, ptr + 8);
    return ptr + 8 + len;
  }
  if (decoder instanceof ArrayDecoder) {
    const array = value as any as any[];
    const len = array.length;
    mem[ptr + 7] = len & 0xFF;
    mem[ptr + 6] = (len >> 8) & 0xFF;
    mem[ptr + 5] = (len >> 16) & 0xFF;
    mem[ptr + 4] = (len >> 24) & 0xFF;
    // mem[ptr + 3] = (len >> 32) & 0xFF;
    // mem[ptr + 2] = (len >> 40) & 0xFF;
    // mem[ptr + 1] = (len >> 48) & 0xFF;
    // mem[ptr] = (len >> 56) & 0xFF;
    let jx = ptr + 8;
    for (let i = 0; i < len; i++) {
      jx = runEncoder(decoder._element, mem, jx, array[i]);
    }
    return jx;
  }
  if (decoder instanceof RecordDecoder) {
    const obj = value as Record<string, any>;
    let jx = ptr;
    for (const k in decoder._description) {
      if (Object.prototype.hasOwnProperty.call(decoder._description, k)) {
        jx = runEncoder(decoder._description[k], mem, jx, obj[k]);
      }
    }
    return jx;
  }
  if (decoder instanceof OneOfDecoder) {
    const tag = (value as any)['tag'];
    const dscrSize = discriminatorSize(Object.keys(decoder._alternatives).length);
    mem[ptr] = tag;
    return runEncoder(decoder._alternatives[tag], mem, ptr + dscrSize, value);
  }
  if (decoder instanceof RecursiveDecoder) {
    return runEncoder(decoder._self, mem, ptr, value);
  }
  if (decoder instanceof TupleDecoder) {
    const tupleVal = value as any[];
    let jx = ptr;
    decoder._tuple.forEach((dec, i) => {
      jx = runEncoder(dec, mem, jx, tupleVal[i]);
    });
    return jx;
  }
  return absurd(decoder);
}

function discriminatorSize(numAlernatives: number): number {
  return Math.ceil(Math.log2(numAlernatives) / 8);
}

function readDiscriminator(dscrSize: number, mem: Uint8Array, ix: HaskellPointer): [number, HaskellPointer] {
  // TODO: complete implementation for dscrSize > 1
  if (dscrSize != 1) throw new Error('Unimplemented');
  return [mem[ix], ix + 1];
}

export const int8 = new Int8Decoder<number>();
export const int16 = new Int16Decoder<number>();
export const int32 = new Int32Decoder<number>();
export const int64 = new Int64Decoder<bigint>();

export const word8 = new Word8Decoder<number>();
export const word16 = new Word16Decoder<number>();
export const word32 = new Word32Decoder<number>();
export const word64 = new Word64Decoder<bigint>();

export const float32 = new Float32Decoder<number>();
export const float64 = new Float64Decoder<number>();

export const string = new StringDecoder<string>();

export const u8array = new Uint8ArrayDecoder<Uint8Array>();

export function array<A>(d: Decoder<A>): ArrayDecoder<A[]> {
  return new ArrayDecoder(d);
}

export function record<fields extends { [k: string]: Decoder<any> }>(fields: fields): RecordDecoder<{[k in keyof fields]: fields[k]['_A'] }> {
  return new RecordDecoder(fields);
}

export type NarrowDownByTag<
  Alternatives extends Record<string|number, Decoder<any>>
  > = OneOfDecoder<{ [K in keyof Alternatives]: { tag: K } & Alternatives[K]['_A']}[keyof Alternatives]>;

export function discriminate<Alternatives extends Record<string|number, Decoder<any>>>(alts: Alternatives): NarrowDownByTag<Alternatives> {
  return new OneOfDecoder(alts);
}

export function tuple<Args extends Decoder<unknown>[]>(...args: Args): TupleDecoder<{[k in keyof Args]: Args[k]['_A'] }>  {
  return new TupleDecoder(args);
}

export function recursive<A>(f: (self: Decoder<any>) => Decoder<A>): Decoder<A> {
  const self = new RecursiveDecoder<A>(undefined as any);
  const result = f(self);
  self._self = result;
  return result;
}

export type Maybe<A>
  = { tag: MaybeTag.Nothing }
  | { tag: MaybeTag.Just, 0: A };

export enum MaybeTag { Nothing, Just };

export function maybe<A>(decoder: Decoder<A>): Decoder<Maybe<A>> {
  return discriminate({
    [MaybeTag.Nothing]: record({}),
    [MaybeTag.Just]: record({ 0: decoder })
  });
}
