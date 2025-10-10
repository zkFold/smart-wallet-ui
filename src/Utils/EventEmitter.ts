import { AppEvent, EventListener } from '../Types'

export class EventEmitter<E> {
  private listeners: Map<E, EventListener<E>[]> = new Map()

  public on(eventId: E, listener: EventListener<E>): void {
    if (!this.listeners.has(eventId)) {
      this.listeners.set(eventId, [])
    }
    this.listeners.get(eventId)!.push(listener)
  }

  public off(eventId: E, listener: EventListener<E>): void {
    const eventListeners = this.listeners.get(eventId)
    if (eventListeners) {
      const index = eventListeners.indexOf(listener)
      if (index > -1) {
        eventListeners.splice(index, 1)
      }
    }
  }

  public emit(eventType: E, data?: any): void {
    const event: AppEvent<E> = { id: eventType, data }
    const eventListeners = this.listeners.get(eventType)
    if (eventListeners) {
      eventListeners.forEach(listener => {
        try {
          listener(event)
        } catch (error) {
          console.error(`Error in event listener for ${eventType}:`, error)
        }
      })
    }
  }

  public removeAllListeners(eventId?: E): void {
    if (eventId) {
      this.listeners.delete(eventId)
    } else {
      this.listeners.clear()
    }
  }
}