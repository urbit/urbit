import React from 'react';
import { Pike } from '@urbit/api';

import { Attribute } from './Attribute';

export function PikeMeta(props: { pike: Pike }) {
  const { pike } = props;

  const pluralUpdates = pike.wefts?.length !== 1;
  return (
    <div className="mt-5 sm:mt-8 space-y-5 sm:space-y-8">
      <Attribute title="Desk Hash" attr="hash">
        {pike.hash}
      </Attribute>
      <Attribute title="Installed into" attr="local-desk">
        %{pike.sync?.desk}
      </Attribute>
      {pike.wefts && pike.wefts.length > 0 ? (
        <Attribute attr="next" title="Pending Updates">
          {pike.wefts.length} update{pluralUpdates ? 's are' : ' is'} pending a System Update
        </Attribute>
      ) : null}
    </div>
  );
}
