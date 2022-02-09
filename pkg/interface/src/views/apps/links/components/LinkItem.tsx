import { Action, Anchor, Box, Col, Icon, Row, Rule, Text } from '@tlon/indigo-react';
import { Association, GraphNode, Group, markEachAsRead, removePosts, TextContent, UrlContent, ReferenceContent } from '@urbit/api';
import React, { ReactElement, RefObject, useCallback, useEffect, useRef } from 'react';
import { Link, Redirect } from 'react-router-dom';
import { roleForShip } from '~/logic/lib/group';
import { getPermalinkForGraph, referenceToPermalink } from '~/logic/lib/permalinks';
import { useCopy } from '~/logic/lib/useCopy';
import { useHarkStat } from '~/logic/state/hark';
import Author from '~/views/components/Author';
import { Dropdown } from '~/views/components/Dropdown';
import RemoteContent from '~/views/components/RemoteContent';
import { PermalinkEmbed } from '../../permalinks/embed';
import airlock from '~/logic/api';
import useSettingsState from '~/logic/state/settings';

interface LinkItemProps {
  node: GraphNode;
  association: Association;
  resource: string;
  group: Group;
  baseUrl: string;
  mt?: number;
  measure?: any;
}
export const LinkItem = React.forwardRef((props: LinkItemProps, ref: RefObject<HTMLDivElement>): ReactElement => {
  const {
    association,
    node,
    resource,
    group,
    ...rest
  } = props;

  if (typeof node.post === 'string' || !node.post) {
    return <Redirect to="/~404" />;
  }

  const { putEntry } = useSettingsState.getState();
  const remoteRef = useRef<HTMLDivElement>(null);
  const setRef = useCallback((el: HTMLDivElement | null ) => {
    remoteRef.current = el;
  }, []);
  const index = node.post.index.split('/')[1];

  const [ship, name] = resource.split('/');
  const harkPath = `/graph/~${ship}/${name}`;
  const markRead = useCallback(() => {
    airlock.poke(
      markEachAsRead({ desk: (window as any).desk, path: harkPath }, `/${index}`)
    );
  }, [resource, index]);

  useEffect(() => {
    setTimeout(() => {
      if(document.activeElement instanceof HTMLIFrameElement
        // @ts-ignore forwardref prop passing
        && remoteRef?.current?.containerRef?.contains(document.activeElement)) {
        markRead();
      }
    });
  }, []);

  const URLparser = new RegExp(
    /((?:([\w\d\.-]+)\:\/\/?){1}(?:(www)\.?){0,1}(((?:[\w\d-]+\.)*)([\w\d-]+\.[\w\d]+))){1}(?:\:(\d+)){0,1}((\/(?:(?:[^\/\s\?]+\/)*))(?:([^\?\/\s#]+?(?:.[^\?\s]+){0,1}){0,1}(?:\?([^\s#]+)){0,1})){0,1}(?:#([^#\s]+)){0,1}/
  );

  const author = node.post.author;
  const size = node.children ? node.children.size : 0;
  const contents = node.post.contents as [TextContent, UrlContent];
  const hostname = URLparser.exec(contents[1].url) ? URLparser.exec(contents[1].url)[4] : null;
  const href = URLparser.exec(contents[1].url) ? contents[1].url : `http://${contents[1].url}`;

  const baseUrl = props.baseUrl || `/~404/${resource}`;

  const ourRole = group ? roleForShip(group, window.ship) : undefined;

  const permalink = getPermalinkForGraph(
    association.group,
    association.resource,
    `/${index}`
  );

  const { doCopy: doCopyLink, copyDisplay: locationText } = useCopy(
    contents[1].url,
    'Copy block source'
  );

  const { doCopy: doCopyNode, copyDisplay: nodeText } = useCopy(
    permalink,
    'Copy reference'
  );

  const deleteLink = () => {
    if (confirm('Are you sure you want to delete this link?')) {
      airlock.poke(removePosts(`~${ship}`, name, [node.post.index]));

      // If the default bookmark title changes, this will have to be found in some other way.
      const permalinkText = node.post.contents.find(({ text }) => text?.includes('web+urbitgraph://'));
      if (permalinkText?.text) {
        putEntry('bookmarks', permalinkText.text, '');
      }
    }
  };

  const linkStats = useHarkStat(harkPath);
  const commStats = useHarkStat(`${harkPath}/${index}`);
  const commColor = commStats.count > 0 ? 'blue' : 'gray';
  const isUnread = linkStats.each.includes(`/${index}`);

  return (
    <Box
      mx="auto"
      px={3}
      maxWidth="768px"
      ref={ref}
      width="100%"
      opacity={node.post.pending ? '0.5' : '1'}
      {...rest}
    >
      <Box
        lineHeight="tall"
        display='flex'
        flexDirection='column'
        width="100%"
        color='washedGray'
        border={1}
        borderColor={isUnread ? 'blue' : 'lightGray'}
        borderRadius={2}
        alignItems="flex-start"
        overflow="hidden"
        onClick={markRead}
      >
        {contents[0].text ? <Text p={2}>{contents[0].text}</Text> : null}
        { 'reference' in contents[1] ? (
          <>
            <Rule />
            <PermalinkEmbed full link={referenceToPermalink(contents[1] as unknown as ReferenceContent).link} transcluded={0} />
          </>
        ) : (
        <>
        <RemoteContent
          embedRef={setRef}
          // @ts-ignore RemoteContent weirdness
          renderUrl={false}
          url={href}
          tall
        />
        <Text color="gray" p={2} flexShrink={0}>
            <Anchor  target="_blank" rel="noopener noreferrer" style={{ textDecoration: 'none' }} href={href}>
              <Box display='flex'>
                <Icon icon='ArrowExternal' mr={1} />{hostname}
              </Box>
            </Anchor>
          </Text>
        </>
      )}
      </Box>
      <Row minWidth={0} flexShrink={0} width="100%" justifyContent="space-between" py={3} bg="white">
      <Author
        showImage
        isRelativeTime
        ship={author}
        date={node.post['time-sent']}
        group={group}
        lineHeight={1}
      />
      <Box ml="auto">
        <Link
          to={node.post.pending ? '#' : `${baseUrl}/index/${index}`}
          style={{ cursor: node.post.pending ? 'default' : 'pointer' }}
        >
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
              <Action bg="white" m={1} color="black" onClick={doCopyLink}>{locationText}</Action>
            </Row>
            <Row alignItems="center" p={1}>
              <Action bg="white" m={1} color="black" onClick={doCopyNode}>{nodeText}</Action>
            </Row>

            {(ourRole === 'admin' || node.post.author === window.ship) &&
              <Row alignItems="center" p={1}>
                <Action bg="white" m={1} color="red" destructive onClick={deleteLink}>Delete Link</Action>
              </Row>
            }
          </Col>
        }
      >
        <Icon ml={2} display="block" icon="Ellipsis" color="gray" />
      </Dropdown>

    </Row>
  </Box>);
});

