import React, { useState, useEffect, useRef, useCallback, ReactElement }  from 'react';
import { Link } from 'react-router-dom';

import { Row, Col, Anchor, Box, Text, Icon, Action } from '@tlon/indigo-react';
import { GraphNode, Group, Rolodex, Unreads } from '@urbit/api';

import { writeText } from '~/logic/lib/util';
import Author from '~/views/components/Author';
import { roleForShip } from '~/logic/lib/group';
import GlobalApi from '~/logic/api/global';
import { Dropdown } from '~/views/components/Dropdown';
import RemoteContent from '~/views/components/RemoteContent';
import useHarkState from '~/logic/state/hark';

interface LinkItemProps {
  node: GraphNode;
  resource: string;
  api: GlobalApi;
  group: Group;
  path: string;
}

export const LinkItem = (props: LinkItemProps): ReactElement => {
  const {
    node,
    resource,
    api,
    group,
    path,
    ...rest
  } = props;

  const ref = useRef<HTMLDivElement | null>(null);
  const remoteRef = useRef<typeof RemoteContent | null>(null);

  const markRead = useCallback(() => {
    api.hark.markEachAsRead(props.association, '/', `/${index}`, 'link', 'link');
  }, [props.association, index]);

  useEffect(() => {
    function onBlur() {
      // FF will only update on next tick
      setTimeout(() => {
        console.log(remoteRef.current);
        if(document.activeElement instanceof HTMLIFrameElement
          && remoteRef?.current?.containerRef?.contains(document.activeElement)) {
          markRead();
        }
      });
    }
    window.addEventListener('blur', onBlur);
    return () => {
      window.removeEventListener('blur', onBlur);
    };
  }, [markRead]);

  const URLparser = new RegExp(
    /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
  );

  const author = node.post.author;
  const index = node.post.index.split('/')[1];
  const size = node.children ? node.children.size : 0;
  const contents = node.post.contents;
  const hostname = URLparser.exec(contents[1].url) ? URLparser.exec(contents[1].url)[4] : null;
  const href = URLparser.exec(contents[1].url) ? contents[1].url : `http://${contents[1].url}`

  const baseUrl = props.baseUrl || `/~404/${resource}`;

  const ourRole = group ? roleForShip(group, window.ship) : undefined;
  const [ship, name] = resource.split('/');

  const [locationText, setLocationText] = useState('Copy Link Location');

  const copyLocation = () => {
    setLocationText('Copied');
    writeText(contents[1].url);
    setTimeout(() => {
      setLocationText('Copy Link Location');
    }, 2000);
  };

  const deleteLink = () => {
    if (confirm('Are you sure you want to delete this link?')) {
      api.graph.removeNodes(`~${ship}`, name, [node.post.index]);
    }
  };

  const appPath = `/ship/~${resource}`;
  const unreads = useHarkState(state => state.unreads);
  const commColor = (unreads.graph?.[appPath]?.[`/${index}`]?.unreads ?? 0) > 0 ? 'blue' : 'gray';
  const isUnread = unreads.graph?.[appPath]?.['/']?.unreads?.has(node.post.index);

  return (
    <Box
      mx="auto"
      px={3}
      maxWidth="768px"
      ref={ref}
      width="100%"
      opacity={node.post.pending ? '0.5' : '1'}
      {...rest}>
      <Box
        lineHeight="tall"
        display='flex'
        flexDirection='column'
        width="100%"
        color='washedGray'
        border={1}
        borderColor={isUnread ? 'blue' : 'washedGray'}
        borderRadius={2}
        alignItems="flex-start"
        overflow="hidden"
        onClick={markRead}
      >
        <Text p={2}>{contents[0].text}</Text>
        <RemoteContent
          ref={r => { remoteRef.current = r }}
          renderUrl={false}
          url={href}
          text={contents[0].text}
          unfold={true}
          style={{ alignSelf: 'center' }}
          oembedProps={{
            p: 2,
            className: 'links embed-container',
            onClick: markRead
          }}
          imageProps={{
            marginLeft: 'auto',
            marginRight: 'auto',
            display: 'block'
          }}
          textProps={{
            overflow: 'hidden',
            color: 'black',
            display: 'block',
            alignSelf: 'center',
            style: { textOverflow: 'ellipsis', whiteSpace: 'pre', width: '100%' },
            p: 2
          }}
        />
        <Text color="gray" p={2} flexShrink={0}>
          <Anchor  target="_blank" rel="noopener noreferrer" style={{ textDecoration: 'none' }} href={href}>
            <Box display='flex'>
              <Icon icon='ArrowExternal' mr={1} />{hostname}
            </Box>
          </Anchor>
        </Text>
      </Box>
      <Row minWidth='0' flexShrink={0} width="100%" justifyContent="space-between" py={3} bg="white">
      <Author
        showImage
        ship={author}
        date={node.post['time-sent']}
        group={group}
      />
      <Box ml="auto">
        <Link
          to={node.post.pending ? '#' : `${baseUrl}/${index}`}
          style={{ cursor: node.post.pending ? 'default' : 'pointer' }}>
        <Box display='flex'>
          <Icon color={commColor} icon='Chat' />
          <Text color={commColor} ml={1}>{size}</Text>
        </Box>
      </Link>
        </Box>

      <Dropdown
        dropWidth="200px"
        alignX="right"
        alignY="top"
        options={
          <Col backgroundColor="white" border={1} borderRadius={1} borderColor="lightGray">
            <Row alignItems="center" p={1}>
              <Action bg="white" m={1} color="black" onClick={copyLocation}>{locationText}</Action>
            </Row>
            {(ourRole === 'admin' || node.post.author === window.ship) &&
              <Row alignItems="center" p={1}>
                <Action bg="white" m={1} color="red" destructive onClick={deleteLink}>Delete Link</Action>
              </Row>
            }
          </Col>
        }
      >
        <Icon ml="2" display="block" icon="Ellipsis" color="gray" />
      </Dropdown>

    </Row>
  </Box>);
};

