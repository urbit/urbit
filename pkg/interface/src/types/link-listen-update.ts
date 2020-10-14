import { Path } from './noun';

interface LinkListenUpdateListening {
  listening: Path[];
}

interface LinkListenUpdateWatch {
  watch: Path;
}

interface LinkListenUpdateLeave {
  leave: Path;
}

export type LinkListenUpdate =
  LinkListenUpdateListening
| LinkListenUpdateWatch
| LinkListenUpdateLeave;
