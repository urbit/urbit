import { Box, Row } from '@tlon/indigo-react';
import moment from 'moment';
import React, { ReactElement, ReactNode } from 'react';
import { PropFunc } from '~/types';
import ProfileOverlay from './ProfileOverlay';
import { ShipImage } from './ShipImage';
import { ShipName } from './ShipName';
import Timestamp from './Timestamp';

export interface AuthorProps {
  ship: string;
  date?: number;
  showImage?: boolean;
  children?: ReactNode;
  unread?: boolean;
  size?: number;
  lineHeight?: string | number;
  isRelativeTime?: boolean;
}

// eslint-disable-next-line max-lines-per-function
function Author(props: AuthorProps & PropFunc<typeof Box>): ReactElement {
  const {
    ship = '',
    date,
    showImage,
    fullNotIcon,
    children,
    unread,
    isRelativeTime,
    dontShowTime,
    lineHeight = 'tall',
    ...rest
  } = props;

  const time = props.time || props.date || false;
  const size = props.size || 16;
  const sigilPadding = props.sigilPadding || 2;

  const stamp = moment(date);

  return (
    <Row {...rest} alignItems='center' width='auto'>
      <Box
        height={`${size}px`}
        overflow='hidden'
        position='relative'
        cursor='pointer'
      >
        {showImage && (
          <ProfileOverlay ship={ship}>
            <ShipImage
              icon={!fullNotIcon}
              ship={`~${ship}`}
              size={size}
              sigilSize={size - (2*sigilPadding)}
            />
          </ProfileOverlay>
        )}
      </Box>
      <Box display='flex' alignItems='baseline'>
        <ShipName ship={`~${ship}`} ml={showImage ? 2 : 0} lineHeight={lineHeight} />
        { !dontShowTime && time && (
          <Timestamp
            height="fit-content"
            relative={isRelativeTime}
            stamp={stamp}
            fontSize={0}
            time={time}
            whiteSpace='nowrap'
            ml={2}
            color={unread ? 'blue' : 'gray'}
          />
        )}
        {children}
      </Box>
    </Row>
  );
}

export default React.memo(Author);
