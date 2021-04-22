# Urbit API in JavaScript

This package simplifies the process of working with Urbit's APIs into fluent, typed functions organized by app. Pairs well with `@urbit/http-api`. Compare:

Without:
```ts
import UrbitInterface from '@urbit/http-api';
const api: UrbitInterface = useApi();
api.poke({
  app: 'settings-store',
  mark: 'settings-event',
  json: {
    'put-entry': {
      'bucket-key': bucket,
      'entry-key': key,
      'value': value
    }
  }
});
```

With:
```ts
import UrbitInterface from '@urbit/http-api';
import { settings } from '@urbit/api';
const api: UrbitInterface = useApi();
api.poke(setings.putEntry(bucket, key, value));
```

You may import single functions
```ts
import { putEntry } from '@urbit/api';
```
or whole apps:
```ts
import { settings } from '@urbit/api';
```

This package also provides types and utilities for working with Urbit's internal data structures, such as Nouns, Das, Tas, and so forth.

This package was originally developed as part of Tlon's Landscape client and therefore the best reference material exists [there](https://github.com/urbit/urbit/tree/master/pkg/interface/src).