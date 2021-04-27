import React, { useCallback, useState, useRef, ReactElement } from 'react';
import _ from 'lodash';
import { Link, Switch, Route } from 'react-router-dom';
import Helmet from 'react-helmet';

import { Box, Icon, Col, Text, Row } from '@tlon/indigo-react';

import { Body } from '~/views/components/Body';
import { PropFunc } from '~/types/util';
import Inbox from './inbox';
import { Dropdown } from '~/views/components/Dropdown';
import { FormikOnBlur } from '~/views/components/FormikOnBlur';
import GroupSearch from '~/views/components/GroupSearch';
import { useTutorialModal } from '~/views/components/useTutorialModal';
import useHarkState from '~/logic/state/hark';
import useMetadataState from '~/logic/state/metadata';
import useGroupState from '~/logic/state/group';
import {StatelessAsyncAction} from '~/views/components/StatelessAsyncAction';

const baseUrl = '/~notifications';

const HeaderLink = React.forwardRef((
  props: PropFunc<typeof Text> & { view?: string; current: string },
  ref
): ReactElement => {
  const { current, view, ...textProps } = props;
  const to = view ? `${baseUrl}/${view}` : baseUrl;
  const active = view ? current === view : !current;

  return (
    <Link to={to}>
      <Text ref={ref} px="2" {...textProps} gray={!active} />
    </Link>
  );
});

interface NotificationFilter {
  groups: string[];
}

export default function NotificationsScreen(props: any): ReactElement {
  const relativePath = (p: string) => baseUrl + p;

  const [filter, setFilter] = useState<NotificationFilter>({ groups: [] });
  const associations = useMetadataState(state => state.associations);
  const pendingJoin = useGroupState(s => s.pendingJoin);
  const onSubmit = async ({ groups } : NotificationFilter) => {
    setFilter({ groups });
  };
  const onReadAll = useCallback(async () => {
    await props.api.hark.readAll();
  }, []);
  const groupFilterDesc =
    filter.groups.length === 0
      ? 'All'
      : filter.groups
          .map(g => associations.groups?.[g]?.metadata?.title)
    .join(', ');
  const anchorRef = useRef<HTMLElement | null>(null);
  useTutorialModal('notifications', true, anchorRef);
  const notificationsCount = useHarkState(state => state.notificationsCount);
  return (
    <Switch>
      <Route
        path={[relativePath('/:view'), relativePath('')]}
        render={(routeProps) => {
          const { view } = routeProps.match.params;
          return (
            <>
              <Helmet defer={false}>
                <title>{ notificationsCount ? `(${String(notificationsCount) }) `: '' }Landscape - Notifications</title>
              </Helmet>
              <Body>
                <Col overflowY="hidden" height="100%">
                  <Row
                    p="3"
                    alignItems="center"
                    height="48px"
                    justifyContent="space-between"
                    width="100%"
                    borderBottom="1"
                    borderBottomColor="lightGray"
                  >

                  <Text fontWeight="bold" fontSize="2" lineHeight="1" ref={anchorRef}>
                    Notifications
                  </Text>
                    <Row
                      justifyContent="space-between"
                      gapX="3"
                    >
                      <StatelessAsyncAction
                        overflow="hidden"
                        color="black"
                        onClick={onReadAll}
                      >
                        Mark All Read
                      </StatelessAsyncAction>
                      <Link to="/~settings#notifications">
                        <Box>
                          <Icon lineHeight="1" icon="Adjust" />
                        </Box>
                      </Link>
                    </Row>
                  </Row>
                  {!view && <Inbox
                    pendingJoin={pendingJoin}
                    {...props}
                    filter={filter.groups} 
                    />}
                </Col>
              </Body>
            </>
          );
        }}
      />
    </Switch>
  );
}
