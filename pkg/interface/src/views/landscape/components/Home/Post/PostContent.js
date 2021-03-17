import React from 'react';
import { Box, Row } from '@tlon/indigo-react';
import TextContent from '~/views/apps/chat/components/content/text';
import CodeContent from '~/views/apps/chat/components/content/code';
import RemoteContent from '~/views/components/RemoteContent';
import { Mention } from '~/views/components/MentionText';


// TODO: deduplicate with chat and probably comments
export function PostContent(props) {
  const { index, node, groups, associations, api, contacts } = props;
  return (
    <Row width="100%">
      {node.post.contents.map((content, i) => {
      switch (Object.keys(content)[0]) {
        case 'text':
          return (
            <TextContent key={i}
              associations={associations}
              groups={groups}
              api={api}
              fontSize={1}
              lineHeight={'20px'}
              content={content}
            />
          );
        case 'code':
          return <CodeContent key={i} content={content} />;
        case 'url':
          return (
            <Box
              key={i}
              flexShrink={0}
              fontSize={1}
              lineHeight='20px'
              color='black'
            >
              <RemoteContent
                key={content.url}
                url={content.url}
                imageProps={{
                  style: {
                    maxWidth: 'min(100%,18rem)',
                    display: 'inline-block',
                    marginTop: '0.5rem'
                  }
                }}
                videoProps={{
                  style: {
                    maxWidth: '18rem',
                    display: 'block',
                    marginTop: '0.5rem'
                  }
                }}
                textProps={{
                  style: {
                    fontSize: 'inherit',
                    borderBottom: '1px solid',
                    textDecoration: 'none'
                  }
                }}
              />
            </Box>
          );
        case 'mention':
          const first = (i) => (i === 0);
          return (
            <Mention
              key={i}
              first={first(i)}
              group={group}
              scrollWindow={scrollWindow}
              ship={content.mention}
              contact={contacts?.[`~${content.mention}`]}
            />
          );
        default:
          return null;
      }
      })
      }
    </Row>
  );
}


