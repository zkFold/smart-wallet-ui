// Policy ID for Smart Wallet utility tokens that should be hidden
const HIDDEN_POLICY_ID = '10a01cd6ba666b392ef1967c7af500377d9241d5414b2d658f2179aa'

export function formatBalance(balance: { [asset: string]: bigint }): string {
  let assets = ""
  for (const [key, value] of Object.entries(balance)) {
    // Skip tokens with the Smart Wallet utility policy ID
    if (key.startsWith(HIDDEN_POLICY_ID)) {
      continue
    }
    
    if (key === 'lovelace') {
      // Convert lovelaces to ADA (1 ADA = 1,000,000 lovelaces)
      const adaValue = Number(value) / 1_000_000
      assets += `<li><b>${adaValue.toFixed(6)}</b> <i>ada</i></li>`
    } else {
      assets += `<li><b>${value}</b> <i>${key}</i></li>`
    }
  }
  if (assets === "") {
    assets = "<li>No assets available</li>"
  }
  return assets
}

export function generateRandomState(): string {
  const array = new Uint8Array(32)
  crypto.getRandomValues(array)
  return Array.from(array, byte => byte.toString(16).padStart(2, '0')).join('')
}

export function harden(num: number): number {
  return 0x80000000 + num
}

export function debounce<T extends (...args: any[]) => void>(
  func: T,
  wait: number
): (...args: Parameters<T>) => void {
  let timeout: NodeJS.Timeout | null = null
  
  return (...args: Parameters<T>) => {
    if (timeout) {
      clearTimeout(timeout)
    }
    
    timeout = setTimeout(() => {
      func(...args)
    }, wait)
  }
}

export function validateEmail(email: string): boolean {
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/
  return emailRegex.test(email)
}

export function validateBech32Address(address: string): boolean {
  // Basic bech32 validation - starts with addr_test or addr
  return /^(addr_test|addr)1[a-z0-9]+$/i.test(address)
}

export function sanitizeInput(input: string): string {
  return input.trim().replace(/[<>\\"'&]/g, '')
}

export function decodeJWT(token: string): any {
  try {
    // JWT has 3 parts separated by dots: header.payload.signature
    const parts = token.split('.')
    if (parts.length !== 3) {
      throw new Error('Invalid JWT format')
    }
    
    // Decode the payload (second part)
    const payload = parts[1]
    // Add padding if needed for base64 decoding
    const paddedPayload = payload + '='.repeat((4 - payload.length % 4) % 4)
    const decoded = atob(paddedPayload.replace(/-/g, '+').replace(/_/g, '/'))
    return JSON.parse(decoded)
  } catch (error) {
    console.error('Failed to decode JWT:', error)
    return null
  }
}
