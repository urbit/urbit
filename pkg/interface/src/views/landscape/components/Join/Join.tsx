import {
  Col,
  Row,
  Text,
  Box,
  Button,
  ManagedTextInputField,
  ManagedCheckboxField,
  ContinuousProgressBar,
<<<<<<< HEAD
} from '@tlon/indigo-react';
import { Formik, Form } from 'formik';
import React, { useEffect, useState } from 'react';
import { useHistory, useLocation } from 'react-router-dom';
import useGroupState from '~/logic/state/group';
import { useInviteForResource } from '~/logic/state/invite';
import useMetadataState, { usePreview } from '~/logic/state/metadata';
import { decline, Invite } from '@urbit/api';
import { join, JoinRequest } from '@urbit/api/groups';
import airlock from '~/logic/api';
import { joinError, joinLoad, JoinProgress } from '@urbit/api';
import { useQuery } from '~/logic/lib/useQuery';
import { JoinKind, JoinDesc, JoinSkeleton } from './Skeleton';
=======
} from "@tlon/indigo-react";
import { Formik, Form } from "formik";
import React, { useEffect } from "react";
import { useHistory, useLocation, useParams } from "react-router-dom";
import useGroupState from "~/logic/state/group";
import useInviteState, { useInviteForResource } from "~/logic/state/invite";
import useMetadataState, { usePreview } from "~/logic/state/metadata";
import { decline, Invite } from "@urbit/api";
import { join, JoinRequest } from "@urbit/api";
import airlock from "~/logic/api";
import { joinError, joinResult, joinLoad, JoinProgress } from "@urbit/api";
import { useQuery } from "~/logic/lib/useQuery";
import { JoinKind, JoinDesc, JoinSkeleton } from "./Skeleton";
>>>>>>> 3a542c635e (fixed couple annoying imports (#1))

interface InviteWithUid extends Invite {
  uid: string;
}

interface FormSchema {
  autojoin: boolean;
  shareContact: boolean;
}

const initialValues = {
  autojoin: false,
  shareContact: false,
};

function JoinForm(props: {
  desc: JoinDesc;
  dismiss: () => void;
  invite?: InviteWithUid;
}) {
  const { desc, dismiss, invite } = props;
  const onSubmit = (values: FormSchema) => {
    const [, , ship, name] = desc.group.split('/');
    airlock.poke(
      join(ship, name, desc.kind, values.autojoin, values.shareContact)
    );
  };

  const onDecline = () => {
    airlock.poke(decline(desc.kind, invite.uid));
    dismiss();
  };
  const isGroups = desc.kind === 'groups';

  return (
    <Formik initialValues={initialValues} onSubmit={onSubmit}>
      <Form>
        <Col p='4' gapY='4'>
          {isGroups ? (
            <ManagedCheckboxField id='autojoin' label='Join all channels' />
          ) : null}
          <ManagedCheckboxField id='shareContact' label='Share identity' />
          <Row justifyContent='space-between' width='100%'>
            <Button onClick={dismiss}>Dismiss</Button>
            <Row gapX='2'>
              {!invite ? null : (
                <Button onClick={onDecline} destructive type='button'>
                  Decline
                </Button>
              )}
              <Button primary type='submit'>
                {!invite ? 'Join Group' : 'Accept'}
              </Button>
            </Row>
          </Row>
        </Col>
      </Form>
    </Formik>
  );
}

export function JoinInitial(props: {
  invite?: InviteWithUid;
  desc: JoinDesc;
  modal: boolean;
  dismiss: () => void;
}) {
  const { desc, dismiss, modal, invite } = props;
  const title = (() => {
    const name = desc.kind === 'graph' ? 'Group Chat' : 'Group';
    if (invite) {
      return `You've been invited to a ${name}`;
    } else {
      return `You're joining a ${name}`;
    }
  })();
  return (
    <JoinSkeleton modal={modal} desc={desc} title={title}>
      <JoinForm invite={invite} dismiss={dismiss} desc={desc} />
    </JoinSkeleton>
  );
}

function JoinLoading(props: {
  desc: JoinDesc;
  modal: boolean;
  request: JoinRequest;
  dismiss: () => void;
  finished: string;
}) {
  const { desc, request, dismiss, modal, finished } = props;
  const history = useHistory();
  useEffect(() => {
    if (desc.kind === 'graph' && request.progress === 'done') {
      history.push(finished);
    }
  }, [request]);
  const name = desc.kind === 'graph' ? 'Group Chat' : 'Group';
  const title = `Joining ${name}, please wait`;
  const onCancel = () => {
    useGroupState.getState().abortJoin(desc.group);
    dismiss();
  };
  return (
    <JoinSkeleton modal={modal} desc={desc} title={title}>
      <Col maxWidth='512px' p='4' gapY='4'>
        {joinLoad.indexOf(request.progress as any) !== -1 ? (
          <JoinProgressIndicator progress={request.progress} />
        ) : null}
        <Box>
          <Text>
            If join seems to take a while, the host of the {name} may be
            offline, or the connection between you both may be unstable.
          </Text>
        </Box>
        <Row gapX='2'>
          <Button onClick={dismiss}>Dismiss</Button>
          <Button destructive onClick={onCancel}>
            Cancel Join
          </Button>
        </Row>
      </Col>
    </JoinSkeleton>
  );
}

function JoinError(props: {
  desc: JoinDesc;
  request: JoinRequest;
  modal: boolean;
  dismiss: () => void;
}) {
  const { dismiss, desc, request, modal } = props;
  const { preview } = usePreview(desc.group);
  const group = preview?.metadata?.title ?? desc.group;
  const title = `Joining ${group} failed`;
  const explanation =
    request.progress === 'no-perms'
      ? 'You do not have the correct permissions'
    : 'An unexpected error occurred';

  const onRetry = () => {
    useGroupState.getState().abortJoin(desc.group);
    const [,,ship,name] = group.split('/');
    airlock.poke(
      join(ship, name, desc.kind, false, false)
    );
  };

  const onAbort = () => {
    useGroupState.getState().abortJoin(desc.group);
    dismiss();
  };
  

  return (
    <JoinSkeleton modal={modal} title={title} desc={desc}>
      <Col p='4' gapY='4'>
        <Text fontWeight='medium'>{explanation}</Text>
        <Row gapX="2">
          <Button onClick={onRetry} primary>Retry</Button>
          <Button onClick={onAbort} destructive>Abort</Button>
        </Row>
      </Col>
    </JoinSkeleton>
  );
}

export interface JoinProps {
  desc: JoinDesc;
  redir?: string;
  modal?: boolean;
  dismiss?: () => void;
}

export function Join(props: JoinProps) {
  const { desc, modal, dismiss, redir } = props;
  const { group, kind } = desc;
  const [, , ship, name] = group.split('/');
  const graph = kind === 'graph';
  const associations = useMetadataState(s => s.associations);
  const joined = graph ? associations.graph[group] : associations.groups[group];
  const finishedPath = redir
    ? redir
    : graph
    ? `/~landscape/messages/resource/chat/${ship}/${name}`
    : `/~landscape/ship/${ship}/${name}`;

  const history = useHistory();
  const joinRequest = useGroupState(s => s.pendingJoin[group]);
  const [openedRequest, setOpenedRequest] = useState<JoinRequest>();
  const invite = useInviteForResource(kind, ship, name);

  const isDone = openedRequest && openedRequest.progress === 'done' && joined;
  const isErrored =
  openedRequest && joinError.includes(openedRequest.progress as any);
  const isLoading =
  openedRequest && joinLoad.includes(openedRequest.progress as any);

  // If we opened this modal from a join request,
  // don't let the request getting deleted move us to the wrong state
  useEffect(() => {
    if (joinRequest) {
      setOpenedRequest(joinRequest);
    }
  }, [joinRequest]);

  useEffect(() => {
    if (isDone && desc.kind == 'graph') {
      history.push(finishedPath);
    }
  }, [isDone, desc]);

  return isDone ? (
    <JoinDone
      dismiss={dismiss}
      modal={modal}
      desc={desc}
      finished={finishedPath}
    />
  ) : isLoading ? (
    <JoinLoading
      modal={modal}
      dismiss={dismiss}
      desc={desc}
      request={openedRequest}
      finished={finishedPath}
    />
  ) : isErrored ? (
    <JoinError dismiss={dismiss} modal={modal} desc={desc} request={openedRequest} />
  ) : (
    <JoinInitial modal={modal} dismiss={dismiss} desc={desc} invite={invite} />
  );
}

interface PromptFormSchema {
  link: string;
}
export interface JoinPromptProps {
  kind: string;
  dismiss?: () => void;
}

export function JoinPrompt(props: JoinPromptProps) {
  const { dismiss } = props;
  const { appendQuery } = useQuery();
  const history = useHistory();
  const initialValues = {
    link: ''
  };

  const onSubmit = async ({ link }: PromptFormSchema) => {
    const path = `/ship/${link}`;
    history.push({
      search: appendQuery({ 'join-path': path })
    });
  };

  return (
    <JoinSkeleton modal body={<Text>a</Text>} title='Join a Group'>
      <Formik initialValues={initialValues} onSubmit={onSubmit}>
        <Form>
          <Col p='4' gapY='4'>
            <ManagedTextInputField
              label='Invite Link'
              id='link'
              caption='Enter either a web+urbitgraph:// link or an identifier in the form ~sampel-palnet/group'
            />
            <Row gapX='2'>
              {dismiss ? (
                <Button type='button' onClick={dismiss}>
                  Dismiss
                </Button>
              ) : null}
              <Button type='submit' primary>
                Join
              </Button>
            </Row>
          </Col>
        </Form>
      </Formik>
    </JoinSkeleton>
  );
}

function JoinProgressIndicator(props: { progress: JoinProgress }) {
  const { progress } = props;
  const percentage =
    progress === 'done' ? 100 : (joinLoad.indexOf(progress as any) + 1) * 25;

  const description = (() => {
    switch (progress) {
      case 'start':
        return 'Connecting to host';
      case 'added':
        return 'Retrieving members';
      case 'metadata':
        return 'Retrieving channels';
      case 'done':
        return 'Finished';
      default:
        return '';
    }
  })();

  return (
    <Col gapY='2'>
      <Text color='lightGray'>{description}</Text>
      <ContinuousProgressBar percentage={percentage} />
    </Col>
  );
}

export interface JoinDoneProps {
  desc: JoinDesc;
  modal: boolean;
  finished: string;
  dismiss: () => void;
}

export function JoinDone(props: JoinDoneProps) {
  const { desc, modal, finished, dismiss } = props;
  const name = desc.kind === 'groups' ? 'Group' : 'Group Chat';
  const title = `Joined ${name} successfully`;
  const history = useHistory();

  const onView = () => {
    history.push(finished);
  };

  return (
    <JoinSkeleton title={title} modal={modal} desc={desc}>
      <Col p='4' gapY='4'>
        <JoinProgressIndicator progress='done' />
        <Row gapX='2'>
          <Button onClick={dismiss}>Dismiss</Button>
          <Button onClick={onView} primary>
            View Group
          </Button>
        </Row>
      </Col>
    </JoinSkeleton>
  );
}

export interface JoinParams extends Record<string, string> {
  'join-kind': JoinKind;
  'join-path'?: string;
  redir?: string;
}

export function createJoinParams(kind: JoinKind, path?: string, redirect?: string, inLink?: true): string;
export function createJoinParams(kind: JoinKind, path?: string, redirect?: string, inLink?: false): JoinParams;
export function createJoinParams(kind: JoinKind, path?: string, redirect?: string, inLink = true) {
  const params = {
    'join-kind': kind
  };

  if (path) {
    params['join-path'] = path;
  }

  if (redirect) {
    params['redir'] = redirect;
  }

  return inLink ? '?' + new URLSearchParams(params).toString() : params;
}

export function JoinRoute() {
  const { query } = useQuery();
  const history = useHistory();
  const { pathname } = useLocation();
  const kind = query.get('join-kind');
  const path = query.get('join-path')?.replace('web+urbitgraph://group/', '');
  const redir = query.get('redir');

  if (!kind) {
    return null;
  }

  const desc: JoinDesc = path
    ? {
        group: path,
        kind: kind === 'graph' ? 'graph' : 'groups'
      }
    : undefined;

  const dismiss = () => {
    history.push(pathname);
  };

  return desc ? (
    <Join desc={desc} modal dismiss={dismiss} redir={redir} />
  ) : (
    <JoinPrompt kind={kind} dismiss={dismiss} />
  );
}
