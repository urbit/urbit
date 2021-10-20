import { createStorageKey } from './state/util';

const key = createStorageKey(`storage-wipe-${import.meta.env.VITE_LAST_WIPE}`);
const wiped = localStorage.getItem(key);

if (!wiped) {
  localStorage.clear();
  localStorage.setItem(key, 'true');
}
