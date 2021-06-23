import React from 'react';
import { BaseImage, Box, Center } from '@tlon/indigo-react';
import { Sigil } from '~/logic/lib/sigil';
import { uxToHex } from '@urbit/api';
import { useContact } from '~/logic/state/contact';
import useSettingsState, { SettingsState } from '~/logic/state/settings';
import { useDark } from '~/logic/state/join';

interface ShipImageProps {
  ship: string;
  size: number;
  icon?: boolean;
  sigilSize?: number;
  borderRadius?: number;
}

const selSettings = (s: SettingsState) => s.calm.hideAvatars;

export function ShipImage(props: ShipImageProps) {
  const { borderRadius = 1, ship, icon = false, size } = props;
  const sigilSize = props.sigilSize ?? size / 2;
  const contact = useContact(ship);
  const hideAvatars = useSettingsState(selSettings);
  const dark = useDark();
  const color = contact?.color
    ? `#${uxToHex(contact?.color)}`
    : dark
    ? '#000000'
    : '#FFFFFF';

  return contact?.avatar && !hideAvatars ? (
    <BaseImage
      referrerPolicy="no-referrer"
      display="inline-block"
      style={{ objectFit: 'cover' }}
      src={contact.avatar}
      height={size}
      width={size}
      borderRadius={borderRadius}
    />
  ) : (
    <Box size={size} borderRadius={borderRadius} backgroundColor={color}>
      <Center width={size} height={size}>
        <Sigil icon={icon} ship={ship} size={sigilSize} color={color} />
      </Center>
    </Box>
  );
}
