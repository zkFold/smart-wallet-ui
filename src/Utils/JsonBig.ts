/**
 * JSON serialization utilities that can handle BigInt and BigIntWrap objects
 * Adapted from the Backend class serialize method from zkfold-smart-wallet-api
 */

const JSONbig = require('json-bigint')

// Configure JSONbig to handle BigInt values properly
const JSONbigConfig = JSONbig({
  storeAsString: false,
  useNativeBigInt: true
})

/**
 * Serialize data using JSONbig to handle BigIntWrap objects properly
 * @param data - Data to serialize
 * @returns JSON string that can handle BigInt values
 */
export function serialize(data: any): string {
  return JSONbigConfig.stringify(data)
}

/**
 * Deserialize JSON string that may contain BigInt values
 * @param jsonString - JSON string to deserialize
 * @returns Parsed object with BigInt values properly restored
 */
export function deserialize(jsonString: string): any {
  return JSONbigConfig.parse(jsonString)
}