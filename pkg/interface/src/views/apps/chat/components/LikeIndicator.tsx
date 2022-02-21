import React from 'react';
import { Box, Icon, Row, Text } from '@tlon/indigo-react';
import { useHovering } from '~/logic/lib/util';
import { IS_MOBILE } from '~/logic/lib/platform';

interface LikeIndicatorProps {
  transcluded: boolean;
  isLiked: boolean;
  didLike: boolean;
  showLikers: boolean;
  dark: boolean;
  likers: string[];
  onLike: () => void;
}

export function LikeIndicator ({
  transcluded,
  isLiked,
  didLike,
  showLikers,
  dark,
  likers,
  onLike
}: LikeIndicatorProps) {
  const { hovering, bind } = useHovering();

  if (transcluded || (!likers.length && !isLiked)) {
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
      {((hovering && !IS_MOBILE) || showLikers) && <Box ml={2}>
        <Text mono fontSize="12px">{likeInfoText}</Text>
      </Box>}
    </Row>
  );
}
