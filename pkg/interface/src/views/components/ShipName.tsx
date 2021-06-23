import { Text } from '@tlon/indigo-react';
import React from 'react';
import { useContact } from '~/logic/state/contact';
import { useCopy } from '~/logic/lib/useCopy';
import useSettingsState, { SettingsState } from '~/logic/state/settings';
import { cite } from '@urbit/api/lib';
import { PropFunc } from '~/types';

const selSettings = (s: SettingsState) => s.calm.hideNicknames;
type ShipNameProps = PropFunc<typeof Text> & {
  ship: string;
  strong?: boolean;
  copiable?: boolean;
};

const noop = () => {};
export function ShipName(props: ShipNameProps) {
  const { ship, strong = false, copiable = false, ...rest } = props;
  const contact = useContact(ship);
  const hideNicknames = useSettingsState(selSettings);

  const showNick = !hideNicknames && contact?.nickname?.length > 0;
  const name = showNick ? contact?.nickname : cite(ship);
  const { copyDisplay, doCopy } = useCopy(ship, name);
  const fontWeight = (strong && showNick) ? '500' : '400';

  return (
    <Text
      fontSize={1}
      textOverflow='ellipsis'
      overflow='hidden'
      whiteSpace='pre'
      mono={!showNick}
      fontFamily={showNick ? 'sans' : 'mono'}
      cursor={copiable ? 'pointer' : 'default'}
      fontWeight={fontWeight}
      onClick={copiable ? doCopy : noop}
      title={showNick ? cite(ship) : contact?.nickname}
      {...rest}
    >
      {copyDisplay}
    </Text>
  );
}
