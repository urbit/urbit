import { BaseImage, Box } from '@tlon/indigo-react';
import { Contact } from '@urbit/api';
import React from 'react';
import { Sigil } from '~/logic/lib/sigil';
import { uxToHex } from '~/logic/lib/util';

interface ChatAvatar {
  hideAvatars: boolean;
  contact?: Contact;
}

export function ChatAvatar({ contact, hideAvatars }) {
  const color = contact ? uxToHex(contact.color) : '000000';
  const sigilClass = contact ? '' : 'mix-blend-diff';

  if (contact && contact?.avatar && !hideAvatars) {
    return (
      <BaseImage
        flexShrink={0}
        src={contact.avatar}
        height={24}
        width={24}
        style={{ objectFit: 'cover' }}
        borderRadius={1}
        display='inline-block'
      />
    );
  }

  return (
    <Box
      width={24}
      height={24}
      display='flex'
      justifyContent='center'
      alignItems='center'
      backgroundColor={`#${color}`}
      borderRadius={1}
    >
      <Sigil
        ship={window.ship}
        size={16}
        color={`#${color}`}
        classes={sigilClass}
        icon
        padding={2}
      />
    </Box>
  );
}
