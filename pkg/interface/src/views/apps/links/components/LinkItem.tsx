import React  from 'react';
import { Link } from 'react-router-dom';
import { Row, Col, Anchor, Box, Text, BaseImage, Icon } from '@tlon/indigo-react';

import { Sigil } from '~/logic/lib/sigil';
import { cite } from '~/logic/lib/util';
import Author from '~/views/components/Author';

import { roleForShip } from '~/logic/lib/group';
import { Contacts, GraphNode, Group, LocalUpdateRemoteContentPolicy, Rolodex } from '~/types';
import GlobalApi from '~/logic/api/global';

interface LinkItemProps {
  node: GraphNode;
  resource: string;
  hideAvatars: boolean;
  hideNicknames: boolean;
  remoteContentPolicy: LocalUpdateRemoteContentPolicy;
  api: GlobalApi;
  group: Group;
  contacts: Rolodex;
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
    contacts,
    ...rest
  } = props;

  const URLparser = new RegExp(
    /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
  );

  const author = node.post.author;
  const index = node.post.index.split('/').join('-');
  const size = node.children ? node.children.size : 0;
  const contents = node.post.contents;
  const hostname = URLparser.exec(contents[1].url) ? URLparser.exec(contents[1].url)[4] : null;

  const baseUrl = props.baseUrl || `/~404/${resource}`;

  const ourRole = group ? roleForShip(group, window.ship) : undefined;
  const [ship, name] = resource.split('/');

  return (
    <Box width="100%" {...rest}>
      
      {/* <Box width="100%">
        <Link to={`${baseUrl}/${index}`}>
          <Text color="gray">{size} comment{size > 1 ? 's' : null}</Text>
        </Link>
        {(ourRole === 'admin' || node.post.author === window.ship)
          && (<Text color='red' ml='2' cursor='pointer' onClick={() => api.graph.removeNodes(`~${ship}`, name, [node.post.index])}>Delete</Text>)}
      </Box> */}
    
    <Anchor
      lineHeight="tall"
      display='flex'
      flexDirection='column'
      style={{ textDecoration: 'none' }}
      href={contents[1].url}
      width="100%"
      target="_blank"
      rel="noopener noreferrer"
      p={2}
      borderColor='black'
      border={1}
      borderRadius={2}
    >
      <Text overflow='hidden' style={{ textOverflow: 'ellipsis', whiteSpace: 'pre' }} mb={2}>{contents[0].text}</Text>
      <Text color="gray" flexShrink={0}><Box display='flex'><Icon icon='ArrowExternal' mr={1}/>{hostname}</Box></Text>
    </Anchor>
    <Row minWidth='0' flexShrink={0} width="100%" justifyContent="space-between" py={3} bg="white">
      
      <Author
        showImage
        contacts={contacts}
        ship={author}
        date={node.post['time-sent']}
        hideAvatars={hideAvatars}
        hideNicknames={hideNicknames}
        remoteContentPolicy={remoteContentPolicy}
        group={group}
        api={api}
      >
        
      </Author>
      <Link to={`${baseUrl}/${index}`}>
        <Box display='flex'>
          <Icon color='blue' icon='Chat' />
          <Text color='blue' ml={1}>{node.children.size}</Text>
        </Box>
      </Link> 
      
    </Row>
    </Box>
  );
};

