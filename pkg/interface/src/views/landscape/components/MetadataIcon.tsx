import { Box, Image } from '@tlon/indigo-react';
import { Metadata } from '@urbit/api';
import React from 'react';
import { uxToHex } from '~/logic/lib/util';
import { PropFunc } from '~/types/util';

type MetadataIconProps = PropFunc<typeof Box> & {
  metadata: Pick<Metadata, 'color' | 'picture'>;
};

export function MetadataIcon(props: MetadataIconProps) {
  const { metadata, ...rest } = props;

  const bgColor = metadata.picture ? {} : { bg: `#${uxToHex(metadata.color)}` };

  return (
    <Box {...bgColor} {...rest} borderRadius={1} boxShadow="inset 0 0 0 1px" color="lightGray" overflow="hidden">
      {metadata.picture && <Image height="100%" src={metadata.picture} />}
    </Box>
  );
}
