import { uxToHex } from '@urbit/api';
import React from 'react';
import { Box, BaseImage } from '@tlon/indigo-react';
import { useContact } from '~/logic/state/contact';
import useSettingsState from '~/logic/state/settings';
import { Sigil } from '~/logic/lib/sigil';

export function ShipImage(props: {
  ship: string;
  size?: number;
  sigilClass?: string;
}) {
  const { ship, size = 24, sigilClass = '' } = props;
  const contact = useContact(ship);
  const { hideAvatars } = useSettingsState(s => s.calm);

  const color = contact?.color ? uxToHex(contact.color) : '000000';

  return contact?.avatar && !hideAvatars ? (
    <BaseImage
      flexShrink={0}
      src={contact.avatar}
      height={size}
      width={size}
      style={{ objectFit: 'cover' }}
      borderRadius={1}
      display="inline-block"
    />
  ) : (
    <Box
      width={24}
      height={24}
      display="flex"
      justifyContent="center"
      alignItems="center"
      backgroundColor={`#${color}`}
      borderRadius={1}
    >
      <Sigil
        ship={ship}
        size={16}
        color={`#${color}`}
        classes={sigilClass}
        icon
        padding={2}
      />
    </Box>
  );
}
