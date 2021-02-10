import React, { useEffect, useState, useLayoutEffect } from "react";

import { Box, Text, Row, Button, Action } from "@tlon/indigo-react";
import GlobalApi from "~/logic/api/global";
import { Associations, Groups } from "~/types";
import { MetadataIcon } from "../landscape/components/MetadataIcon";
import {JoinGroup} from "../landscape/components/JoinGroup";
import {useModal} from "~/logic/lib/useModal";

export function GroupLink(props: {
  api: GlobalApi;
  resource: string;
  associations: Associations;
  groups: Groups;
  measure: () => void;
}) {
  const { resource, api, measure } = props;
  const name = resource.slice(6);
  const [preview, setPreview] = useState<MetadataUpdatePreview | null>(null);

  const { modal, showModal } = useModal({
    modal: (
      <JoinGroup
        groups={props.groups}
        associations={props.associations}
        api={api}
        autojoin={name}
      />
    )
  });

  const joined = resource in props.associations.groups;

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
    <Box>
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
            <MetadataIcon height="4" width="4"  metadata={preview.metadata} />
            <Text ml="2" fontWeight="medium">{preview.metadata.title}</Text>
            <Button disabled={joined} onClick={showModal} ml="4" primary>
              {joined ? "Joined" : "Join"}
            </Button>
          </>
        ) : (
          <Text mono>{name}</Text>
        )}
      </Row>
    </Box>
  );
}
