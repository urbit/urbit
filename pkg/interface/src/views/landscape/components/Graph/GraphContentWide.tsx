import React from "react";
import { Post, ReferenceContent } from "@urbit/api";
import { Box } from "@tlon/indigo-react";

import GlobalApi from "~/logic/api/global";
import TextContent from "./content/text";
import CodeContent from "./content/code";
import RemoteContent from "~/views/components/RemoteContent";
import { Mention } from "~/views/components/MentionText";
import { PermalinkEmbed } from "~/views/apps/permalinks/embed";
import { referenceToPermalink } from "~/logic/lib/permalinks";
import { PropFunc } from "~/types";

function GraphContentWideInner(
  props: {
    transcluded?: number;
    post: Post;
    api: GlobalApi;
    showOurContact: boolean;
    allowHeaders?: boolean;
    allowLists?: boolean;
  } & PropFunc<typeof Box>
) {
  const { post, transcluded = 0, showOurContact, api, allowHeaders, allowLists, ...rest } = props;

  return (
    <Box {...rest}>
      {post.contents.map((content, i) => {
        switch (Object.keys(content)[0]) {
          case "text":
            return (
              <TextContent
                key={i}
                api={api}
                fontSize={1}
                lineHeight={"20px"}
                content={content}
                allowHeaders={allowHeaders}
                allowLists={allowLists}
              />
            );
          case "code":
            return <CodeContent key={i} content={content} />;
          case "reference":
            const { link } = referenceToPermalink(content as ReferenceContent);
            return (
              <PermalinkEmbed
                link={link}
                api={api}
                transcluded={transcluded}
                showOurContact={showOurContact}
              />
            );
          case "url":
            return (
              <Box
                key={i}
                flexShrink={0}
                fontSize={1}
                lineHeight="20px"
                color="black"
                width="fit-content"
                maxWidth="min(500px, 100%)"
              >
                <RemoteContent key={content.url} url={content.url} />
              </Box>
            );
          case "mention":
            const first = (i) => i === 0;
            return (
              <Mention
                key={i}
                first={first(i)}
                ship={content.mention}
                api={api}
              />
            );
          default:
            return null;
        }
      })}
    </Box>
  );
}

export const GraphContentWide = React.memo(GraphContentWideInner);
