import { createStorageKey } from './logic/lib/util';

const key = createStorageKey(`storage-wipe-${process.env.LANDSCAPE_LAST_WIPE}`);
const wiped = localStorage.getItem(key);

// Loaded before everything, this clears local storage just once.
// Change VITE_LAST_WIPE in .env to date of wipe

if (!wiped) {
  localStorage.clear();
  localStorage.setItem(key, 'true');
}
