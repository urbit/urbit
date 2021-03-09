import React, { useCallback } from "react";
import {
  Col,
  Text,
  ManagedToggleSwitchField as Toggle,
} from "@tlon/indigo-react";
import { Form, FormikHelpers } from "formik";
import { FormikOnBlur } from "~/views/components/FormikOnBlur";
import { BackButton } from "./BackButton";
import GlobalApi from "~/logic/api/global";
import useHarkState from "~/logic/state/hark";
import _ from "lodash";

interface FormSchema {
  mentions: boolean;
  dnd: boolean;
  watchOnSelf: boolean;
}

export function NotificationPreferences(props: {
  api: GlobalApi;
}) {
  const { api } = props;
  const dnd = useHarkState(state => state.doNotDisturb);
  const graphConfig = useHarkState(state => state.notificationsGraphConfig);
  const initialValues = {
    mentions: graphConfig.mentions,
    dnd: dnd,
    watchOnSelf: graphConfig.watchOnSelf,
  };

  const onSubmit = useCallback(async (values: FormSchema, actions: FormikHelpers<FormSchema>) => {
    try {
      let promises: Promise<any>[] = [];
      if (values.mentions !== graphConfig.mentions) {
        promises.push(api.hark.setMentions(values.mentions));
      }
      if (values.watchOnSelf !== graphConfig.watchOnSelf) {
        promises.push(api.hark.setWatchOnSelf(values.watchOnSelf));
      }
      if (values.dnd !== dnd && !_.isUndefined(values.dnd)) {
        promises.push(api.hark.setDoNotDisturb(values.dnd))
      }

      await Promise.all(promises);
      actions.setStatus({ success: null });
      actions.resetForm({ values: initialValues });
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: e.message });
    }
  }, [api]);

  return (
    <>
    <BackButton/>
    <Col p="5" pt="4" gapY="5">
      <Col gapY="1" mt="0">
        <Text fontSize="2" fontWeight="medium">
          Notification Preferences
        </Text>
        <Text gray>
          Set notification visibility and default behaviours for groups and
          messaging
        </Text>
      </Col>
      <FormikOnBlur initialValues={initialValues} onSubmit={onSubmit}>
        <Form>
          <Col gapY="4">
            <Toggle
              label="Do not disturb"
              id="dnd"
              caption="You won't see the notification badge, but notifications will still appear in your inbox."
            />
            <Toggle
              label="Watch for replies"
              id="watchOnSelf"
              caption="Automatically follow a post for notifications when it's yours"
            />
            <Toggle
              label="Watch for mentions"
              id="mentions"
              caption="Notify me if someone mentions my @p in a channel I've joined"
            />
          </Col>
        </Form>
      </FormikOnBlur>
    </Col>
    </>
  );
}
