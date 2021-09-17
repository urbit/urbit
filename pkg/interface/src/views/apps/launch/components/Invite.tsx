import React, { useState, useEffect, useCallback } from "react";
import { useHistory, useParams } from "react-router-dom";
import {
  Box,
  Col,
  Row,
  Text,
  Button,
  Action,
  LoadingSpinner,
} from "@tlon/indigo-react";
import * as Dialog from "@radix-ui/react-dialog";
import { PropFunc } from "~/types";
import { useRunIO } from "~/logic/lib/useRunIO";
import useMetadataState from "~/logic/state/metadata";
import { GroupSummary } from "~/views/landscape/components/GroupSummary";
import {
  accept,
  decline,
  GraphConfig,
  Invite as IInvite,
  join,
  Metadata,
  MetadataUpdatePreview,
  resourceFromPath,
} from "@urbit/api";
import useInviteState from "~/logic/state/invite";
import useGroupState from "~/logic/state/group";
import useGraphState from "~/logic/state/graph";
import { useWaitForProps } from "~/logic/lib/useWaitForProps";
import airlock from "~/logic/api";

function InviteDialog({ children, ...rest }: PropFunc<typeof Col>) {
  return (
    <Dialog.Root open>
      <Dialog.Overlay asChild>
        <Box
          backgroundColor="scales.black20"
          left="0px"
          top="0px"
          width="100%"
          height="100%"
          position="fixed"
          display="flex"
          zIndex={10}
          justifyContent="center"
          alignItems="center"
          p="4"
        ></Box>
      </Dialog.Overlay>
      <Dialog.DialogContent asChild>
        <Col
          left="0px"
          top="0px"
          width="100%"
          height="100%"
          position="fixed"
          zIndex={10}
          justifyContent="center"
          alignItems="center"
        >
          <Col
            p="4"
            gapY="4"
            border="1"
            borderColor="lightGray"
            backgroundColor="white"
            {...rest}
          >
            {children}
          </Col>
        </Col>
      </Dialog.DialogContent>
    </Dialog.Root>
  );
}

function inviteUrl(
  hidden: boolean,
  resource: string,
  metadata?: Metadata<GraphConfig>
) {
  if (!hidden) {
    return `/~landscape${resource}`;
  }

  if (metadata.config?.graph === "chat") {
    return `/~landscape/messages/resource/${metadata.config.graph}${resource}`;
  } else {
    return `/~landscape/home/resource/${metadata.config?.graph}${resource}`;
  }
}

function useInviteAccept(resource: string, app?: string, uid?: string) {
  const { ship, name } = resourceFromPath(resource);
  const history = useHistory();
  const associations = useMetadataState((s) => s.associations);
  const groups = useGroupState((s) => s.groups);
  const graphKeys = useGraphState((s) => s.graphKeys);

  const waiter = useWaitForProps({ associations, graphKeys, groups });
  return useRunIO<void, boolean>(
    async () => {
      if (!(app && uid)) {
        return false;
      }
      if (resource in groups) {
        await airlock.poke(decline(app, uid));
        return false;
      }

      await airlock.poke(join(ship, name));
      await airlock.poke(accept(app, uid));
      await waiter((p) => {
        return (
          (resource in p.groups &&
            resource in (p.associations?.graph ?? {}) &&
            p.graphKeys.has(resource.slice(7))) ||
          resource in (p.associations?.groups ?? {})
        );
      });
      return true;
    },
    (success: boolean) => {
      if (!success) {
        return;
      }
      const redir = inviteUrl(
        groups?.[resource]?.hidden,
        resource,
        associations?.graph?.[resource]?.metadata
      );
      if (redir) {
        // weird race condition
        setTimeout(() => {
          history.push(redir);
        }, 200);
      }
    },
    resource
  );
}

export function Invite() {
  const { uid, app } = useParams<{
    app: string;
    uid: string;
  }>();

  const invite = useInviteState((s) => s.invites?.[app]?.[uid]);

  return (
    <InviteDialog>
      {!!invite ? (
        <>
          {renderInviteContent(app, uid, invite)}
          <InviteActions app={app} uid={uid} invite={invite} />
        </>
      ) : (
        <LoadingSpinner />
      )}
    </InviteDialog>
  );
}

function InviteActions({
  app,
  uid,
  invite,
}: {
  app: string;
  uid: string;
  invite: IInvite;
}) {
  const { push } = useHistory();
  const { ship, name } = invite.resource;
  const resource = `/ship/~${ship}/${name}`;

  const inviteAccept = useInviteAccept(resource, app, uid);
  const acceptInvite = () => inviteAccept();

  const declineInvite = useCallback(async () => {
    if (!(app && uid)) {
      return;
    }
    await airlock.poke(decline(app, uid));
    push('/');
  }, [app, uid]);

  return (
    <Row gapX="4">
      <Action onClick={acceptInvite} backgroundColor="white" color="blue">
        Accept
      </Action>
      <Action onClick={declineInvite} backgroundColor="white" destructive>
        Decline
      </Action>
    </Row>
  );
}

function GroupInvite({ uid, invite }: { uid: string; invite: IInvite }) {
  const {
    resource: { ship, name },
  } = invite;
  const { associations, getPreview } = useMetadataState();
  const [preview, setPreview] = useState<MetadataUpdatePreview | null>(null);
  useEffect(() => {
    (async () => {
      setPreview(await getPreview(`/ship/~${ship}/${name}`));
    })();

    return () => {
      setPreview(null);
    };
  }, [ship, name]);

  return preview ? (
    <GroupSummary
      metadata={preview.metadata}
      channelCount={preview["channel-count"]}
      memberCount={preview["members"]}
    />
  ) : (
    <LoadingSpinner />
  );
}

function GraphInvite({ uid, invite }: { uid: string; invite: IInvite }) {
  return <Text>You have been invited to a group chat by {invite.ship}</Text>;
}

function renderInviteContent(app: string, uid: string, invite: IInvite) {
  switch (app) {
    case "groups":
      return <GroupInvite uid={uid} invite={invite} />;
    case "graph":
      return <GraphInvite uid={uid} invite={invite} />;
    default:
      return null;
  }
}
