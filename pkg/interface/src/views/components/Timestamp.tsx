import moment, { Moment as MomentType } from 'moment';
import React, { ReactElement } from 'react';

import { Box, BoxProps, Text } from '@tlon/indigo-react';
import { useHovering, MOMENT_CALENDAR_DATE, MOMENT_RELATIVE_TIME } from '~/logic/lib/util';

export const DateFormat = 'YYYY.M.D';
export const TimeFormat = 'HH:mm';

export type TimestampProps = BoxProps & {
  stamp: MomentType;
  date?: boolean;
  time?: boolean;
  relativeTime?: boolean;
};

const Timestamp = (props: TimestampProps): ReactElement | null => {
  const {
    relativeTime = false,
    stamp, 
    date, 
    time = true, 
    color = 'gray', 
    fontSize = 0, 
    ...rest
  } = props;
  if (!stamp) return null;
  const { hovering, bind } = useHovering();
  let datestamp = stamp.calendar(MOMENT_CALENDAR_DATE);
  const timestamp = relativeTime 
    ? stamp.calendar(null, MOMENT_RELATIVE_TIME as any) 
    : stamp.format(TimeFormat);
  return (
    <Box
      {...bind}
      display='flex'
      flex='row'
      flexWrap='nowrap'
      {...rest}
      title={stamp.format(DateFormat + ' ' + TimeFormat)}
    >
      {time && (
        <Text flexShrink={0} color={color} fontSize={fontSize}>
          {timestamp}
        </Text>
      )}
      {(hovering || date) && (
        <Text
          flexShrink={0}
          color={color}
          fontSize={fontSize}
          display={time ? ['none', hovering ? 'block' : 'none'] : 'block'}
        >
          {time ? '\u00A0' : ''}
          {datestamp}
        </Text>
      )}
    </Box>
  );
};

export default Timestamp;
