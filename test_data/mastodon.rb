require_relative './mastodon/config/application'
require 'activerecord/lib/active_record/persistence'
require_relative './mastodon/app/lib/atom_serializer'

$id_counter = 0
$model_log = Array.create

module ActiveRecord

  module Persistence

    module ClassMethods

      def _create_record(attribute_names=self.attribute_names)
        id = $id_counter
        $id_counter += 1
        $model_log << self
        yield(self) if block_given?
        id
      end

      def _update_record(attribute_names=self.attribute_names)
        @_trigger_update_callback = true
        yield(self) if block_given?
        if attribute_names.empty?
          0
        else
          1
        end
      end

    end

  end

end


module MastodonBridge

  def _account_atom(account, posts)
    AtomSerializer.render(AtomSerializer.new.feed(account, posts))
  end

  def _created_accounts()
    $model_log.filter do |model|

    end
  end

end
