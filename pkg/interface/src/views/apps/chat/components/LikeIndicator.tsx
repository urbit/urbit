import React from 'react';
import { Box, Icon, Row, Text } from '@tlon/indigo-react';
import { useHovering } from '~/logic/lib/util';

interface LikeIndicatorProps {
  transcluded: boolean;
  isLiked: boolean;
  didLike: boolean;
  dark: boolean;
  likers: string[];
  onLike: () => void;
}

export function LikeIndicator ({
  transcluded,
  isLiked,
  didLike,
  dark,
  likers,
  onLike
}: LikeIndicatorProps) {
  const { hovering, bind } = useHovering();

  if (transcluded || !likers.length) {
    return null;
  }

  const likeInfoText = `Liked by: ${likers.slice(0, 40).join(', ')}${likers.length > 40 ? ` and ${likers.length - 40} more` : ''}`;

  return (
    <Row
      onClick={onLike}
      backgroundColor={isLiked ? 'washedBlue' : 'washedGray'}
      border={dark ? '1px solid white' : undefined}
      width="fit-content" p="1px 4px"
      m="4px 0px 0px 44px"
      position="relative"
      cursor="pointer"
      borderRadius={2}
      {...bind}
    >
      <Icon icon="CheckmarkBold" size={20} color={isLiked ? 'rgba(33,157,255,1)' : 'black'} />
      <Text color={isLiked ? 'rgba(33,157,255,1)' : undefined}>{likers.length + (isLiked && !didLike ? 1 : 0)}</Text>
      {hovering && <Box ml={2}>
        <Text mono fontSize="12px">{likeInfoText}</Text>
      </Box>}
    </Row>
  );
}
