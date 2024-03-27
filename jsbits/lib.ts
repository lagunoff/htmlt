
/** Helper for totality checking */
export function absurd(_x: never): any {
  throw new Error('absurd: unreachable code');
}

export class IntMap<T> {
  public counter: number;
  public map: Map<number, T>;

  constructor() {
    this.counter = 0;
    this.map = new Map<number, T>();
  }

  /**
   * Generates a new key using the internal counter and maps it to the provided value.
   * @param value The value to be mapped to the new key.
   * @returns The generated key.
   */
  push(value: T): number {
    const key = this.counter++;
    this.map.set(key, value);
    return key;
  }

  /**
   * Sets the provided key to the specified value.
   * @param key The key to set.
   * @param value The value to be associated with the key.
   */
  set(key: number, value: T): void {
    this.map.set(key, value);
  }

  /**
   * Gets the value associated with the provided key.
   * @param key The key to retrieve.
   * @returns The value associated with the key, or undefined if the key is not found.
   */
  get(key: number): T | undefined {
    return this.map.get(key);
  }

  /**
   * Deletes the key and its associated value from the map.
   * @param key The key to delete.
   * @returns True if the key was found and deleted, false otherwise.
   */
  delete(key: number): boolean {
    return this.map.delete(key);
  }

  /**
   * Checks if the map contains the specified key.
   * @param key The key to check.
   * @returns True if the key is present in the map, false otherwise.
   */
  has(key: number): boolean {
    return this.map.has(key);
  }

  /**
   * Clears all keys and values from the map.
   */
  clear(): void {
    this.map.clear();
  }

  /**
   * Returns an array of all keys in the map.
   * @returns An array containing all keys in the map.
   */
  keys(): IterableIterator<number> {
    return this.map.keys();
  }

  /**
   * Returns an array of all values in the map.
   * @returns An array containing all values in the map.
   */
  values(): IterableIterator<T> {
    return this.map.values();
  }

  /**
   * Returns an array of key-value pairs in the map.
   * @returns An array containing key-value pairs in the map.
   */
  entries(): IterableIterator<[number, T]> {
    return this.map.entries();
  }

  /**
   * Executes a provided function once per each key/value pair in the Map, in insertion order.
   */
  forEach(callbackfn: (value: T, key: number, map: Map<number, T>) => void, thisArg?: any): void {
    this.map.forEach(callbackfn, thisArg);
  }
}
