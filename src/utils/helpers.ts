export function formatBalance(balance: { [asset: string]: bigint }): string {
  let assets = ""
  for (const [key, value] of Object.entries(balance)) {
    assets += `<li><b>${value}</b> <i>${key}</i></li>`
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
  return input.trim().replace(/[<>\"'&]/g, '')
}