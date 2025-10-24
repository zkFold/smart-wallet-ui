export function getAddressLabel(address: string): string {
  return address.length > 32 ? address.slice(0, 24) + '...' + address.slice(-8) : address
}