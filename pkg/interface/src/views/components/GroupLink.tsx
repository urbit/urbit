import React, { useEffect, useState, useLayoutEffect } from "react";

import { Box, Text, Row, Button, Action } from "@tlon/indigo-react";
import GlobalApi from "~/logic/api/global";
import { Associations, Groups, PropFunc } from "~/types";
import { MetadataIcon } from "../landscape/components/MetadataIcon";
import { JoinGroup } from "../landscape/components/JoinGroup";
import { useModal } from "~/logic/lib/useModal";
import { GroupSummary } from "../landscape/components/GroupSummary";

export function GroupLink(
  props: {
    api: GlobalApi;
    resource: string;
    associations: Associations;
    groups: Groups;
    measure: () => void;
    detailed?: boolean;
  } & PropFunc<typeof Row>
) {
  const { resource, api, associations, groups, measure, ...rest } = props;
  const name = resource.slice(6);
  const [preview, setPreview] = useState<MetadataUpdatePreview | null>(null);

  const joined = resource in props.associations.groups;

  const { modal, showModal } = useModal({
    modal:
      joined && preview ? (
        <Box width="fit-content" p="4">
          <GroupSummary
            metadata={preview.metadata}
            memberCount={preview.members}
            channelCount={preview?.["channel-count"]}
          />
        </Box>
      ) : (
        <JoinGroup
          groups={groups}
          associations={associations}
          api={api}
          autojoin={name}
        />
      ),
  });

  useEffect(() => {
    (async () => {
      setPreview(await api.metadata.preview(resource));
    })();

    return () => {
      setPreview(null);
    };
  }, [resource]);

  useLayoutEffect(() => {
    measure();
  }, [preview]);

  return (
    <Box {...rest}>
      {modal}
      <Row
        width="fit-content"
        flexShrink={1}
        alignItems="center"
        border="1"
        borderColor="lightGray"
        borderRadius="2"
        py="2"
        px="2"
      >
        {preview ? (
          <>
            <MetadataIcon height="4" width="4" metadata={preview.metadata} />
            <Text ml="2" fontWeight="medium">
              {preview.metadata.title}
            </Text>
            <Button onClick={showModal} ml="4" primary>
              {joined ? "View" : "Join"}
            </Button>
          </>
        ) : (
          <Text mono>{name}</Text>
        )}
      </Row>
    </Box>
  );
}
