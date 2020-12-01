import React, { useState }  from 'react';
import { Link } from 'react-router-dom';
import { Row, Col, Anchor, Box, Text, BaseImage, Icon, Action } from '@tlon/indigo-react';

import { Sigil } from '~/logic/lib/sigil';
import { writeText } from '~/logic/lib/util';
import Author from '~/views/components/Author';

import { roleForShip } from '~/logic/lib/group';
import { Contacts, GraphNode, Group, LocalUpdateRemoteContentPolicy, Rolodex } from '~/types';
import GlobalApi from '~/logic/api/global';
import { Dropdown } from '~/views/components/Dropdown';
import RemoteContent from '~/views/components/RemoteContent';

interface LinkItemProps {
  node: GraphNode;
  resource: string;
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  api: GlobalApi;
  group: Group;
  path: string;
  contacts: Rolodex[];
}

export const LinkItem = (props: LinkItemProps) => {
  const {
    node,
    resource,
    hideAvatars,
    hideNicknames,
    remoteContentPolicy,
    api,
    group,
    path,
    contacts,
    ...rest
  } = props;

  const URLparser = new RegExp(
    /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
  );

  const author = node.post.author;
  const index = node.post.index.split('/')[1];
  const size = node.children ? node.children.size : 0;
  const contents = node.post.contents;
  const hostname = URLparser.exec(contents[1].url) ? URLparser.exec(contents[1].url)[4] : null;

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

  return (
    <Box width="100%" {...rest}>
    
      <Box
        lineHeight="tall"
        display='flex'
        flexDirection='column'
        width="100%"
        color='washedGray'
        border={1}
        borderRadius={2}
        alignItems="flex-start"
        overflow="hidden"
      >
        <RemoteContent
          url={contents[1].url}
          text={contents[0].text}
          remoteContentPolicy={remoteContentPolicy}
          unfold={true}
          style={{ alignSelf: 'center' }}
          oembedProps={{
            p: 2,
            className: 'links embed-container',
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
          }} />
        <Text color="gray" p={2} flexShrink={0}>
          <Anchor target="_blank" rel="noopener noreferrer" style={{ textDecoration: 'none' }} href={contents[1].url}>
            <Box display='flex'>
              <Icon icon='ArrowExternal' mr={1} />{hostname}
            </Box>
          </Anchor>
        </Text>
      </Box>
      
      <Row minWidth='0' flexShrink={0} width="100%" justifyContent="space-between" py={3} bg="white">
      
      <Author
        showImage
        contacts={contacts[path]}
        ship={author}
        date={node.post['time-sent']}
        hideAvatars={hideAvatars}
        hideNicknames={hideNicknames}
        remoteContentPolicy={remoteContentPolicy}
        group={group}
        api={api}
      ></Author>

      <Box ml="auto" mr={1}>
        <Link to={`${baseUrl}/${index}`}>
        <Box display='flex'>
          <Icon color='blue' icon='Chat' />
          <Text color='blue' ml={1}>{node.children.size}</Text>
        </Box>
      </Link>
        </Box>
        
      <Dropdown
        width="200px"
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
        <Icon display="block" icon="Ellipsis" color="gray" />
      </Dropdown>
      
    </Row>
  </Box>);
};

