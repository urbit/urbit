import { Row, Col, BaseInput, Action } from '@tlon/indigo-react';
import React, { ChangeEvent, useCallback, useState } from 'react';
import styled from 'styled-components';
import { PropFunc } from '~/types';
import { ShipImage } from '~/views/components/ShipImage';

export interface LinkInputProps {
  url?: string;
  title?: string;
}

const GradientRow = styled(Row)`
  background: linear-gradient(
    to right,
    ${p => p.theme.colors.none},
    ${p => p.theme.colors.white} 50%
  );
`;

const Input = (props: PropFunc<typeof BaseInput>) => (
  <BaseInput
    {...props}
    lineHeight="tall"
    backgroundColor="white"
    color="black"
    fontFamily="sans"
    fontWeight="500"
    fontSize="1"
    flexGrow={1}
  />
);

export function LinkInput(props: LinkInputProps) {
  const [url, setUrl] = useState(props.url || '');
  const [title, setTitle] = useState(props.title || '');

  const onUrlChange = useCallback((e: ChangeEvent<HTMLInputElement>) => {
    setUrl(e.target.value);
  }, []);

  const onTitleChange = useCallback((e: ChangeEvent<HTMLInputElement>) => {
    setTitle(e.target.value);
  }, []);

  return (
    <Col border="1" borderColor="lightGray" borderRadius="1">
      <Row borderBottom="1" borderBottomColor="lightGray" p="2" gapX="2">
        <ShipImage ship={window.ship} />
        <Input
          placeholder="Add a title"
          value={title}
          onChange={onTitleChange}
        />
      </Row>
      <Row position="relative" width="100%" alignItems="center" p="2" gapX="2">
        <Input
          placeholder="Paste a link here"
          value={url}
          onChange={onUrlChange}
        />
        <GradientRow
          alignItems="center"
          p="2"
          pl="7"
          height="100%"
          right="0"
          top="0"
          position="absolute"
        >
          <Action backgroundColor="white" disabled={url.length === 0}>
            Post
          </Action>
        </GradientRow>
      </Row>
    </Col>
  );
}
