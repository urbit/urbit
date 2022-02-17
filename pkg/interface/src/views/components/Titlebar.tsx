import React, { ReactNode, useCallback, useState } from 'react';
import { Icon, Box, Text } from '@tlon/indigo-react';
import { Link } from 'react-router-dom';
import styled from 'styled-components';
import RichText from '~/views/components/RichText';
import urbitOb from 'urbit-ob';
import { useResize } from '~/logic/lib/useResize';

interface TitlebarProps {
  children?: ReactNode;
  description?: string;
  title: string;
  monoDescription?: boolean;
  workspace: string;
  baseUrl: string;
  back?: string;
}

const TruncatedText = styled(RichText)`
  white-space: nowrap;
  text-overflow: ellipsis;
  overflow: hidden;
`;

export function Titlebar(props: TitlebarProps) {
  const { workspace, monoDescription = false, baseUrl, children, back } = props;
  const [actionsWidth, setActionsWidth] = useState(0);
  const bind = useResize<HTMLDivElement>(
    useCallback((entry) => {
      setActionsWidth(entry.target.getBoundingClientRect().width);
    }, [])
  );
  
  const menuControl = (
    <Link to={`${baseUrl}/settings`}>
      <Icon icon="Menu" color="gray" pr={2} />
    </Link>
  );

  const title = (
    <Text
      mono={urbitOb.isValidPatp(props.title)}
      fontSize={2}
      fontWeight="600"
      textOverflow="ellipsis"
      overflow="hidden"
      whiteSpace="nowrap"
      minWidth={0}
      maxWidth={props?.description ? ['100%', '50%'] : 'none'}
      mr="2"
      ml="1"
      flexShrink={1}
    >
      {props.title}
    </Text>
  );

  const description = (
    <TruncatedText
      display={['none', 'inline']}
      mono={monoDescription}
      color="gray"
      mb={0}
      minWidth={0}
      maxWidth="50%"
      flexShrink={1}
      disableRemoteContent
    >
      {props.description}
    </TruncatedText>
  );

  const backLink = (
    <Box
      borderRight={1}
      borderRightColor="gray"
      pr={3}
      fontSize={1}
      mr="12px"
      my={1}
      flexShrink={0}
      display={back ? 'block' : ['block', 'none']}
    >
      <Link to={back || workspace}>
        <Text>{'<- Back'}</Text>
      </Link>
    </Box>
  );

  return (
    <Box
      flexShrink={0}
      height="48px"
      py={2}
      px={2}
      borderBottom={1}
      borderBottomColor="lightGray"
      display="flex"
      justifyContent="space-between"
      alignItems="center"
    >
      <Box
        display="flex"
        alignItems="baseline"
        width={`calc(100% - ${actionsWidth}px - 16px)`}
        flexShrink={0}
      >
        {backLink}
        {title}
        {description}
      </Box>
      <Box ml={3} display="flex" alignItems="center" flexShrink={0} {...bind}>
        {children}
        {menuControl}
      </Box>
    </Box>
  );
}
