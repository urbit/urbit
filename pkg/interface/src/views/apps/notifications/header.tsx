import { Box, Row, Text as NormalText } from '@tlon/indigo-react';
import moment from 'moment';
import React, { ReactElement } from 'react';
import { PropFunc } from '~/types/util';
import Author from '~/views/components/Author';
import Dot from '~/views/components/Dot';
import Timestamp from '~/views/components/Timestamp';

const Text = (props: PropFunc<typeof Text>) => (
  <NormalText fontWeight="500" {...props} />
);

export function Header(
  props: {
    channelTitle?: string;
    groupTitle?: string;
    description: string;
    time?: number;
    authors?: string[];
    content?: boolean;
  } & PropFunc<typeof Row>
): ReactElement {
  const {
    description,
    channelTitle = '',
    groupTitle,
    authors = [],
    content = false,
    time
  } = props;

  return (
    <Row
      flexDirection={['column-reverse', 'row']}
      minHeight="4"
      mb={content ? 2 : 0}
      onClick={props.onClick}
      flexWrap="wrap"
      alignItems={['flex-start', 'center']}
      gridArea="header"
      overflow="hidden"
    >
      <Row gapX="1" overflow="hidden" alignItems="center">
        {authors.length > 0 && (
          <>
            <Author
              flexShrink={0}
              sigilPadding={6}
              size={24}
              dontShowTime
              date={time}
              ship={authors[0]}
              showImage
            />
            {authors.length > 1 && (
              <Text lineHeight="tall">+ {authors.length - 1} more</Text>
            )}
          </>
        )}
        <Box whiteSpace="nowrap" overflow="hidden" textOverflow="ellipsis">
          <Text lineHeight="tall" mr="1">
            {description} {channelTitle}
          </Text>
        </Box>
      </Row>
      <Row ml={[0, 1]} mb={[1, 0]} gapX="1" alignItems="center">
        {groupTitle && (
          <>
            <Text lineHeight="tall" fontSize="1" gray>
              {groupTitle}
            </Text>
            <Dot color="gray" />
          </>
        )}
        {time && (
          <Timestamp
            lineHeight="tall"
            fontSize="1"
            relative
            stamp={moment(time)}
            color="gray"
            date={false}
          />
        )}
      </Row>
    </Row>
  );
}
