import React, { useCallback } from "react";

import { Box, Col, ManagedCheckboxField as Checkbox } from "@tlon/indigo-react";
import { Formik, Form, FormikHelpers } from "formik";
import * as Yup from "yup";
import _ from "lodash";
import { AsyncButton } from "~/views/components/AsyncButton";
import { FormikOnBlur } from "~/views/components/FormikOnBlur";
import { NotificationGraphConfig } from "~/types";
import GlobalApi from "~/logic/api/global";

interface FormSchema {
  mentions: boolean;
  dnd: boolean;
  watchOnSelf: boolean;
  watching: string[];
}

interface NotificationPreferencesProps {
  graphConfig: NotificationGraphConfig;
  dnd: boolean;
  api: GlobalApi;
}

export default function NotificationPreferences(
  props: NotificationPreferencesProps
) {
  const { graphConfig, api, dnd } = props;

  const initialValues: FormSchema = {
    mentions: graphConfig.mentions,
    watchOnSelf: graphConfig.watchOnSelf,
    dnd,
    watching: graphConfig.watching,
  };

  const onSubmit = useCallback(
    async (values: FormSchema, actions: FormikHelpers<FormSchema>) => {
      console.log(values);
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
    },
    [api, graphConfig]
  );

  return (
    <FormikOnBlur
      initialValues={initialValues}
      onSubmit={onSubmit}
    >
      <Form>
        <Col maxWidth="384px" p="3" gapY="4">
          <Checkbox
            label="Do not disturb"
            id="dnd"
            caption="You won't see the notification badge, but notifications will still appear in your inbox."
          />
          <Checkbox
            label="Watch for replies"
            id="watchOnSelf"
            caption="Automatically follow a post for notifications when it's yours"
          />
          <Checkbox
            label="Watch for mentions"
            id="mentions"
            caption="Notify me if someone mentions my @p in a channel I've joined"
          />
        </Col>
      </Form>
    </FormikOnBlur>
  );
}
