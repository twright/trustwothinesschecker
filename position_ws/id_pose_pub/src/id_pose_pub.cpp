
#include <chrono>
#include <functional>
#include <memory>
#include <string>
#include <optional>
#include <mutex>

#include "rclcpp/rclcpp.hpp"
#include "std_msgs/msg/string.hpp"
#include <nav_msgs/msg/odometry.hpp>
#include <geometry_msgs/msg/pose.hpp>
#include "id_pose_msgs/msg/id_pose.hpp"

using namespace std::chrono_literals;

/* This example creates a subclass of Node and uses std::bind() to register a
 * member function as a callback from the timer. */

class IdPosePublisher : public rclcpp::Node
{
public:
  IdPosePublisher()
      : Node("id_pose_publisher")
  {
    subscription_ = this->create_subscription<nav_msgs::msg::Odometry>(
        "odom", rclcpp::SensorDataQoS(), std::bind(&IdPosePublisher::odom_callback, this, std::placeholders::_1));
    location_publisher_ = this->create_publisher<id_pose_msgs::msg::IdPose>("robot_pose", rclcpp::SensorDataQoS());
    // Create a timer to publish the pose every 500ms
    timer_ = this->create_wall_timer(
        std::chrono::milliseconds(500), std::bind(&IdPosePublisher::publish_pose, this));
  }

private:
  void odom_callback(const nav_msgs::msg::Odometry::SharedPtr msg)
  {
    std::lock_guard<std::mutex> lock(mutex_);
    last_pose_ = msg->pose.pose;
  }

  void publish_pose()
  {
    std::lock_guard<std::mutex> lock(mutex_);
    if (!last_pose_)
    {
      RCLCPP_WARN(this->get_logger(), "No odometry data received yet");
      return;
    }

    // Publish the latest pose
    geometry_msgs::msg::Pose pose_msg = *last_pose_;
    id_pose_msgs::msg::IdPose loc{};
    id_pose_msgs::msg::IdPose loc_msg{};
    loc_msg.id = this->get_namespace();
    loc_msg.pose = *last_pose_;
    location_publisher_->publish(loc_msg);

    RCLCPP_INFO(this->get_logger(), "Published  Pose: [Position: [x: %.2f, y: %.2f, z: %.2f], Orientation: [x: %.2f, y: %.2f, z: %.2f, w: %.2f]]", pose_msg.position.x, pose_msg.position.y, pose_msg.position.z, pose_msg.orientation.x, pose_msg.orientation.y, pose_msg.orientation.z, pose_msg.orientation.w);
  }

  rclcpp::Subscription<nav_msgs::msg::Odometry>::SharedPtr subscription_;
  rclcpp::Publisher<id_pose_msgs::msg::IdPose>::SharedPtr location_publisher_;
  rclcpp::TimerBase::SharedPtr timer_;
  std::optional<geometry_msgs::msg::Pose> last_pose_;
  std::mutex mutex_; // Mutex in case we want multi-threaded executors in future
};

int main(int argc, char *argv[])
{
  rclcpp::init(argc, argv);
  rclcpp::spin(std::make_shared<IdPosePublisher>());
  rclcpp::shutdown();
  return 0;
}
