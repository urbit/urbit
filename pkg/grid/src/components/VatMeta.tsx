import React from 'react';
import moment from 'moment';
import { Vat } from '@urbit/api/hood';

import { Attribute } from './Attribute';

export function VatMeta(props: { vat: Vat }) {
  const { vat } = props;
  const { desk, arak, cass, hash } = vat;
  const { desk: foreignDesk, ship, next } = arak;
  const pluralUpdates = next.length !== 1;
  return (
    <div className="mt-5 sm:mt-8 space-y-5 sm:space-y-8">
      <Attribute title="Developer Desk" attr="desk">
        {ship}/{foreignDesk}
      </Attribute>
      <Attribute title="Last Software Update" attr="case">
        {moment(cass.da).format('YYYY.MM.DD')}
      </Attribute>
      <Attribute title="Desk Hash" attr="hash">
        {hash}
      </Attribute>
      <Attribute title="Installed into" attr="local-desk">
        %{desk}
      </Attribute>
      {next.length > 0 ? (
        <Attribute attr="next" title="Pending Updates">
          {next.length} update{pluralUpdates ? 's are' : ' is'} pending a System Update
        </Attribute>
      ) : null}
    </div>
  );
}
